//! memory の登録・削除と、Vectorize による意味検索。
//!
//! 受け取った本文を CHUNK_SIZE 文字ごとに分割し、各 chunk を OpenAI の
//! text-embedding-3-small で埋め込みベクトルに変換して、Vectorize(binding: VECTORIZE)に
//! chunk_id をキーとして格納する。本文・メタデータは D1 に保存する。
//! 検索時はクエリ文を同じモデルで埋め込み、Vectorize で近傍を引いてから
//! D1 で本文・メタデータを解決して返す。

use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use udamanami_shared::{Memory, MemoryId, MemoryListItem, MemorySearchResult, UpdateMemory};
use worker::js_sys;
use worker::wasm_bindgen::prelude::*;
use worker::wasm_bindgen::{JsCast, JsValue};
use worker::wasm_bindgen_futures::JsFuture;
use worker::*;

#[derive(Deserialize, Serialize)]
struct Chunk {
    chunk_id: String,
    memory_id: String,
    chunk_index: usize,
    content: String,
    embedding: Vec<f32>,
}

const CHUNK_SIZE: usize = 500; // 500文字ごとに分割してD1に保存する

// ---------------- Vectorize のバインディング ----------------
// worker 0.8.5 には Vectorize のラッパーがないので作成

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(extends = js_sys::Object)]
    type VectorizeIndex;

    #[wasm_bindgen(method, catch)]
    fn upsert(
        this: &VectorizeIndex,
        vectors: JsValue,
    ) -> std::result::Result<js_sys::Promise, JsValue>;

    #[wasm_bindgen(method, catch)]
    fn query(
        this: &VectorizeIndex,
        vector: JsValue,
        options: JsValue,
    ) -> std::result::Result<js_sys::Promise, JsValue>;

    #[wasm_bindgen(method, catch, js_name = deleteByIds)]
    fn delete_by_ids(
        this: &VectorizeIndex,
        ids: JsValue,
    ) -> std::result::Result<js_sys::Promise, JsValue>;
}

impl EnvBinding for VectorizeIndex {
    // 実行環境ごとにコンストラクタ名が揺れるため、名前検査はせず素通しでキャストする。
    const TYPE_NAME: &'static str = "VectorizeIndex";

    fn get(val: JsValue) -> Result<Self> {
        Ok(val.unchecked_into())
    }
}

fn js_err(context: &str, e: JsValue) -> Error {
    Error::RustError(format!("{context}: {e:?}"))
}

fn wb_err(context: &str, e: serde_wasm_bindgen::Error) -> Error {
    Error::RustError(format!("{context}: {e}"))
}

fn vectorize(ctx: &RouteContext<()>) -> Result<VectorizeIndex> {
    ctx.env.get_binding::<VectorizeIndex>("VECTORIZE")
}

// ---------------- Vectorize に渡す / から受け取る形 ----------------

/// Vectorize に upsert するベクトルの既定の形: { id, values, metadata?, namespace? } に合わせる
#[derive(Serialize)]
struct VectorizeVector {
    #[serde(rename = "id")]
    chunk_id: String,
    values: Vec<f32>,
    metadata: VectorMeta,
}

/// 任意が可能
#[derive(Serialize)]
struct VectorMeta {
    memory_id: String,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct QueryOptions {
    top_k: u32,
}

#[derive(Deserialize)]
struct QueryResult {
    matches: Vec<QueryMatch>,
}

#[derive(Deserialize)]
struct QueryMatch {
    #[serde(rename = "id")]
    chunk_id: String,
    score: f64,
}

// ---------------- OpenAI 埋め込み ----------------

#[derive(Deserialize)]
struct EmbeddingResponse {
    data: Vec<EmbeddingDatum>,
}

#[derive(Deserialize)]
struct EmbeddingDatum {
    index: usize,
    embedding: Vec<f32>,
}

/// 複数の入力文をまとめて埋め込む。返り値は入力と同じ順序。
async fn embed(api_key: &str, inputs: &[String]) -> Result<Vec<Vec<f32>>> {
    let body = serde_json::json!({
        "model": "text-embedding-3-small",
        "input": inputs,
    });

    let headers = Headers::new();
    headers.set("Authorization", &format!("Bearer {api_key}"))?;
    headers.set("Content-Type", "application/json")?;

    let mut init = RequestInit::new();
    init.with_method(Method::Post)
        .with_headers(headers)
        .with_body(Some(JsValue::from_str(&body.to_string())));

    let req = Request::new_with_init("https://api.openai.com/v1/embeddings", &init)?;
    let mut resp = Fetch::Request(req).send().await?;

    if resp.status_code() != 200 {
        let text = resp.text().await.unwrap_or_default();
        return Err(Error::RustError(format!(
            "OpenAI embeddings failed ({}): {text}",
            resp.status_code()
        )));
    }

    let parsed: EmbeddingResponse = resp.json().await?;
    // index 順に並べ替えてから embedding だけ取り出す。
    let mut data = parsed.data;
    data.sort_by_key(|d| d.index);
    Ok(data.into_iter().map(|d| d.embedding).collect())
}

// ---------------- chunk 生成・埋め込みの共通処理 ----------------

/// 本文を CHUNK_SIZE 文字ごとに分割し、各 chunk を OpenAI でまとめて埋め込む。
/// 本文が空の場合は空の Vec を返す(呼び出し側で 400 を返すこと)。
async fn build_embedded_chunks(
    api_key: &str,
    memory_id: &str,
    content: &str,
) -> Result<Vec<Chunk>> {
    let contents = content.chars().collect::<Vec<char>>();
    let mut chunks: Vec<Chunk> = vec![];
    let mut start = 0;
    let mut idx = 0;

    while start < contents.len() {
        let end = std::cmp::min(start + CHUNK_SIZE, contents.len());
        let chunk_content: String = contents[start..end].iter().collect();
        chunks.push(Chunk {
            chunk_id: uuid::Uuid::new_v4().to_string(),
            memory_id: memory_id.to_string(),
            chunk_index: idx,
            content: chunk_content,
            embedding: vec![], // このあと OpenAI でまとめて埋める。
        });
        start = end;
        idx += 1;
    }

    if chunks.is_empty() {
        return Ok(chunks);
    }

    let inputs: Vec<String> = chunks.iter().map(|c| c.content.clone()).collect();
    let embeddings = embed(api_key, &inputs).await?;
    for (chunk, embedding) in chunks.iter_mut().zip(embeddings) {
        chunk.embedding = embedding;
    }
    Ok(chunks)
}

/// INSERT する memory_chunk 行の prepared statement を組み立てる。
fn insert_chunk_stmts(d1: &D1Database, chunks: &[Chunk]) -> Result<Vec<D1PreparedStatement>> {
    chunks
        .iter()
        .map(|chunk| {
            d1.prepare(
                "INSERT INTO memory_chunk (chunk_id, memory_id, chunk_index, content)
                 VALUES (?, ?, ?, ?)",
            )
            .bind(&[
                chunk.chunk_id.clone().into(),
                chunk.memory_id.clone().into(),
                // D1 は BigInt(Rust の i64→JS BigInt)を受け付けないため、
                // JS の Number になる f64 でバインドする。INTEGER カラムへは整数として入る。
                (chunk.chunk_index as f64).into(),
                chunk.content.clone().into(),
            ])
        })
        .collect()
}

/// chunk 群を Vectorize に upsert する。
async fn upsert_vectors(ctx: &RouteContext<()>, chunks: Vec<Chunk>) -> Result<()> {
    let vectors: Vec<VectorizeVector> = chunks
        .into_iter()
        .map(|chunk| VectorizeVector {
            chunk_id: chunk.chunk_id,
            values: chunk.embedding,
            metadata: VectorMeta {
                memory_id: chunk.memory_id,
            },
        })
        .collect();

    let index = vectorize(ctx)?;
    let js = serde_wasm_bindgen::to_value(&vectors).map_err(|e| wb_err("serialize vectors", e))?;
    let promise = index
        .upsert(js)
        .map_err(|e| js_err("vectorize.upsert", e))?;
    JsFuture::from(promise)
        .await
        .map_err(|e| js_err("await vectorize.upsert", e))?;
    Ok(())
}

/// ある memory_id に紐づく全 embeddings を Vectorize から削除する。
/// D1 側の memory_chunk 行はここでは消さない(呼び出し側で削除・再作成すること)。
async fn delete_vectors_of_memory(
    d1: &D1Database,
    ctx: &RouteContext<()>,
    memory_id: &str,
) -> Result<()> {
    #[derive(Deserialize)]
    struct ChunkIdRow {
        chunk_id: String,
    }
    let rows = d1
        .prepare("SELECT chunk_id FROM memory_chunk WHERE memory_id = ?")
        .bind(&[memory_id.into()])?
        .run()
        .await?
        .results::<ChunkIdRow>()?;

    if rows.is_empty() {
        return Ok(());
    }

    let ids: Vec<String> = rows.into_iter().map(|r| r.chunk_id).collect();
    let index = vectorize(ctx)?;
    let js = serde_wasm_bindgen::to_value(&ids).map_err(|e| wb_err("serialize ids", e))?;
    let promise = index
        .delete_by_ids(js)
        .map_err(|e| js_err("vectorize.deleteByIds", e))?;
    JsFuture::from(promise)
        .await
        .map_err(|e| js_err("await vectorize.deleteByIds", e))?;
    Ok(())
}

// ---------------- ハンドラ ----------------

/// メモリ1件を登録する。本文を chunk 分割し、D1 に保存して各 chunk を Vectorize に upsert する。
pub(super) async fn create_memory(mut req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let body = req.text().await?;
    let Ok(memory) = serde_json::from_str::<Memory>(&body) else {
        return Response::error("Failed to parse request body", 400);
    };

    let memory_id = uuid::Uuid::new_v4().to_string();
    let api_key = ctx.env.secret("OPENAI_API_KEY")?.to_string();
    let chunks = build_embedded_chunks(&api_key, &memory_id, &memory.content).await?;

    if chunks.is_empty() {
        return Response::error("memory content must not be empty", 400);
    }

    // D1 に memory とその chunk を書き込む。
    let d1 = ctx.env.d1("DB")?;
    let timestamp = Date::now().to_string();
    let mut stmts = Vec::with_capacity(chunks.len() + 1);
    stmts.push(
        d1.prepare("INSERT INTO memory (memory_id, title, timestamp) VALUES (?, ?, ?)")
            .bind(&[
                memory_id.clone().into(),
                memory.title.into(),
                timestamp.into(),
            ])?,
    );
    stmts.extend(insert_chunk_stmts(&d1, &chunks)?);
    let _ = d1.batch(stmts).await?;

    // Vectorize に upsert する。
    upsert_vectors(&ctx, chunks).await?;

    Response::from_json(&serde_json::json!({ "memory_id": memory_id }))
}

/// 既存メモリ1件を新しい内容で置き換える。古いベクトルを消したうえで、同じ memory_id の
/// memory 行を消して(chunk は CASCADE で連鎖削除)新しい内容で作り直す。
pub(super) async fn update_memory(mut req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let body = req.text().await?;
    let Ok(update) = serde_json::from_str::<UpdateMemory>(&body) else {
        return Response::error("Failed to parse request body", 400);
    };

    let d1 = ctx.env.d1("DB")?;

    // 対象メモリの存在確認。無ければ 404。
    let exists = d1
        .prepare("SELECT memory_id FROM memory WHERE memory_id = ?")
        .bind(&[update.memory_id.clone().into()])?
        .first::<serde_json::Value>(None)
        .await?
        .is_some();
    if !exists {
        return Response::error("Memory not found", 404);
    }

    let api_key = ctx.env.secret("OPENAI_API_KEY")?.to_string();
    let chunks = build_embedded_chunks(&api_key, &update.memory_id, &update.content).await?;

    if chunks.is_empty() {
        return Response::error("memory content must not be empty", 400);
    }

    // 先に古いベクトルを Vectorize から消す(D1 の chunk 行はまだ残っている)。
    delete_vectors_of_memory(&d1, &ctx, &update.memory_id).await?;

    // D1 を作り直す: memory 行を消す(memory_chunk は CASCADE で連鎖削除される)。
    // その後、同じ memory_id で memory とその chunk を入れ直す。
    let timestamp = Date::now().to_string();
    let mut stmts = Vec::with_capacity(chunks.len() + 2);
    stmts.push(
        d1.prepare("DELETE FROM memory WHERE memory_id = ?")
            .bind(&[update.memory_id.clone().into()])?,
    );
    stmts.push(
        d1.prepare("INSERT INTO memory (memory_id, title, timestamp) VALUES (?, ?, ?)")
            .bind(&[
                update.memory_id.clone().into(),
                update.title.into(),
                timestamp.into(),
            ])?,
    );
    stmts.extend(insert_chunk_stmts(&d1, &chunks)?);
    let _ = d1.batch(stmts).await?;

    // 新しいベクトルを Vectorize に upsert する。
    upsert_vectors(&ctx, chunks).await?;

    Response::from_json(&serde_json::json!({ "memory_id": update.memory_id }))
}

/// メモリ1件を削除する。Vectorize から該当 chunk のベクトルを消し、D1 からも削除する
/// (memory_chunk は ON DELETE CASCADE で連鎖削除される)。
pub(super) async fn delete_memory(mut req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let body = req.text().await?;
    let Ok(memory_id) = serde_json::from_str::<MemoryId>(&body) else {
        return Response::error("Failed to parse request body", 400);
    };

    let d1 = ctx.env.d1("DB")?;

    // Vectorize から消すために、先に chunk_id を集める。
    #[derive(Deserialize)]
    struct ChunkIdRow {
        chunk_id: String,
    }
    let rows = d1
        .prepare("SELECT chunk_id FROM memory_chunk WHERE memory_id = ?")
        .bind(&[memory_id.clone().into()])?
        .run()
        .await?
        .results::<ChunkIdRow>()?;

    if !rows.is_empty() {
        let ids: Vec<String> = rows.into_iter().map(|r| r.chunk_id).collect();
        let index = vectorize(&ctx)?;
        let js = serde_wasm_bindgen::to_value(&ids).map_err(|e| wb_err("serialize ids", e))?;
        let promise = index
            .delete_by_ids(js)
            .map_err(|e| js_err("vectorize.deleteByIds", e))?;
        JsFuture::from(promise)
            .await
            .map_err(|e| js_err("await vectorize.deleteByIds", e))?;
    }

    let _ = d1
        .prepare("DELETE FROM memory WHERE memory_id = ?")
        .bind(&[memory_id.clone().into()])?
        .run()
        .await?;

    Response::ok("Memory deleted successfully")
}

/// メモリを検索する。クエリ文を埋め込み、Vectorize で近傍を引き、D1 で本文・メタデータを解決して返す。
///
/// クエリパラメータ: q (必須), limit (任意, 既定 5)
pub(super) async fn search_memory(req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let url = req.url()?;
    let param = |key: &str| {
        url.query_pairs()
            .find(|(k, _)| k == key)
            .map(|(_, v)| v.into_owned())
    };

    let Some(q) = param("q").filter(|s| !s.is_empty()) else {
        return Response::error("Missing query parameter: q", 400);
    };
    let limit = param("limit")
        .and_then(|v| v.parse::<u32>().ok())
        .unwrap_or(5)
        .clamp(1, 100);

    let api_key = ctx.env.secret("OPENAI_API_KEY")?.to_string();

    // 1. クエリをembedする。
    let mut embeddings = embed(&api_key, &[q]).await?;
    let query_vec = embeddings.remove(0);

    // 2. Vectorize で近傍を引く。
    let index = vectorize(&ctx)?;
    let js_vec =
        serde_wasm_bindgen::to_value(&query_vec).map_err(|e| wb_err("serialize query", e))?;
    let js_opts = serde_wasm_bindgen::to_value(&QueryOptions { top_k: limit })
        .map_err(|e| wb_err("serialize options", e))?;
    let promise = index
        .query(js_vec, js_opts)
        .map_err(|e| js_err("vectorize.query", e))?;
    let js_result = JsFuture::from(promise)
        .await
        .map_err(|e| js_err("await vectorize.query", e))?;
    let result: QueryResult =
        serde_wasm_bindgen::from_value(js_result).map_err(|e| wb_err("parse query result", e))?;

    if result.matches.is_empty() {
        return Response::from_json(&Vec::<MemorySearchResult>::new());
    }

    // 3. chunk_id → score の対応を保持しつつ、D1 で本文とメタデータを解決する。
    let scores: HashMap<String, f64> = result
        .matches
        .iter()
        .map(|m| (m.chunk_id.clone(), m.score))
        .collect();

    let placeholders = std::iter::repeat("?")
        .take(result.matches.len())
        .collect::<Vec<_>>()
        .join(",");
    let binds: Vec<JsValue> = result
        .matches
        .iter()
        .map(|m| m.chunk_id.clone().into())
        .collect();

    let d1 = ctx.env.d1("DB")?;
    #[derive(Deserialize)]
    struct Row {
        chunk_id: String,
        memory_id: String,
        chunk_index: i64,
        content: String,
        title: String,
        timestamp: String,
    }
    let rows = d1
        .prepare(format!(
            "SELECT c.chunk_id, c.memory_id, c.chunk_index, c.content, m.title, m.timestamp
             FROM memory_chunk c
             JOIN memory m ON c.memory_id = m.memory_id
             WHERE c.chunk_id IN ({placeholders})"
        ))
        .bind(&binds)?
        .run()
        .await?
        .results::<Row>()?;

    let mut results: Vec<MemorySearchResult> = rows
        .into_iter()
        .map(|r| MemorySearchResult {
            score: scores.get(&r.chunk_id).copied().unwrap_or(0.0),
            chunk_id: r.chunk_id,
            memory_id: r.memory_id,
            chunk_index: r.chunk_index,
            content: r.content,
            title: r.title,
            timestamp: r.timestamp,
        })
        .collect();

    // Vectorize の類似度が高い順に並べる。
    results.sort_by(|a, b| {
        b.score
            .partial_cmp(&a.score)
            .unwrap_or(std::cmp::Ordering::Equal)
    });

    Response::from_json(&results)
}

/// メモリの一覧を返す。Vectorize で近傍を引くのではなく、D1 に保存されている memory の一覧を返す。
pub(super) async fn list_memories(_req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let d1 = ctx.env.d1("DB")?;

    let rows = d1
        .prepare("SELECT memory_id, title, timestamp FROM memory ORDER BY timestamp DESC")
        .run()
        .await?
        .results::<MemoryListItem>()?;

    Response::from_json(&rows)
}

/// メモリの内容を返す。Vectorize で近傍を引くのではなく、D1 に保存されている memory の内容を返す。
/// chunk を chunk_index 順に連結して本文を復元する。
pub(super) async fn get_memory(req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let url = req.url()?;
    let Some(memory_id) = url
        .query_pairs()
        .find(|(k, _)| k == "memory_id")
        .map(|(_, v)| v.into_owned())
    else {
        return Response::error("Missing query parameter: memory_id", 400);
    };
    let d1 = ctx.env.d1("DB")?;

    // 1. memory 本体(タイトル・時刻)を引く。無ければ 404。
    let Some(meta) = d1
        .prepare("SELECT memory_id, title, timestamp FROM memory WHERE memory_id = ?")
        .bind(&[memory_id.clone().into()])?
        .first::<MemoryListItem>(None)
        .await?
    else {
        return Response::error("Memory not found", 404);
    };

    // 2. chunk を index 順に連結して本文を復元する。
    #[derive(Deserialize)]
    struct ChunkContentRow {
        content: String,
    }
    let chunks = d1
        .prepare(
            "SELECT content FROM memory_chunk
             WHERE memory_id = ? ORDER BY chunk_index ASC",
        )
        .bind(&[memory_id.into()])?
        .run()
        .await?
        .results::<ChunkContentRow>()?;
    let content = chunks
        .into_iter()
        .map(|c| c.content)
        .collect::<Vec<_>>()
        .join("");

    Response::from_json(&serde_json::json!({
        "memory_id": meta.memory_id,
        "title": meta.title,
        "timestamp": meta.timestamp,
        "content": content,
    }))
}
