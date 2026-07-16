//! memory の登録・削除と、Vectorize による意味検索。
//!
//! 受け取った本文を CHUNK_SIZE 文字ごとに分割し、各 chunk を OpenAI の
//! text-embedding-3-small で埋め込みベクトルに変換して、Vectorize(binding: VECTORIZE)に
//! chunk_id をキーとして格納する。本文・メタデータは D1 に保存する。
//! 検索時はクエリ文を同じモデルで埋め込み、Vectorize で近傍を引いてから
//! D1 で本文・メタデータを解決して返す。

use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use udamanami_shared::{DeleteMemory, Memory, MemorySearchResult};
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

// ---------------- ハンドラ ----------------

/// メモリ1件を登録する。本文を chunk 分割し、D1 に保存して各 chunk を Vectorize に upsert する。
pub(super) async fn create_memory(mut req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let body = req.text().await?;
    let Ok(memory) = serde_json::from_str::<Memory>(&body) else {
        return Response::error("Failed to parse request body", 400);
    };

    let mut chunks: Vec<Chunk> = vec![];

    let contents = memory.content.chars().collect::<Vec<char>>();
    let mut start = 0;
    let mut idx = 0;
    let memory_id = uuid::Uuid::new_v4().to_string();

    while start < contents.len() {
        let end = std::cmp::min(start + CHUNK_SIZE, contents.len());
        let chunk_content: String = contents[start..end].iter().collect();
        let chunk = Chunk {
            chunk_id: uuid::Uuid::new_v4().to_string(),
            memory_id: memory_id.clone(),
            chunk_index: idx,
            content: chunk_content,
            embedding: vec![], // このあと OpenAI でまとめて埋める。
        };
        chunks.push(chunk);
        start = end;
        idx += 1;
    }

    if chunks.is_empty() {
        return Response::error("memory content must not be empty", 400);
    }

    // chunk 本文をまとめて埋め込む。
    let api_key = ctx.env.secret("OPENAI_API_KEY")?.to_string();
    let inputs: Vec<String> = chunks.iter().map(|c| c.content.clone()).collect();
    let embeddings = embed(&api_key, &inputs).await?;
    for (chunk, embedding) in chunks.iter_mut().zip(embeddings) {
        chunk.embedding = embedding;
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
    for chunk in &chunks {
        stmts.push(
            d1.prepare(
                "INSERT INTO memory_chunk (chunk_id, memory_id, chunk_index, content)
                 VALUES (?, ?, ?, ?)",
            )
            .bind(&[
                chunk.chunk_id.clone().into(),
                chunk.memory_id.clone().into(),
                (chunk.chunk_index as i64).into(),
                chunk.content.clone().into(),
            ])?,
        );
    }
    let _ = d1.batch(stmts).await?;

    // 2. Vectorize に upsert する。
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

    let index = vectorize(&ctx)?;
    let js = serde_wasm_bindgen::to_value(&vectors).map_err(|e| wb_err("serialize vectors", e))?;
    let promise = index
        .upsert(js)
        .map_err(|e| js_err("vectorize.upsert", e))?;
    JsFuture::from(promise)
        .await
        .map_err(|e| js_err("await vectorize.upsert", e))?;

    Response::from_json(&serde_json::json!({ "memory_id": memory_id }))
}

/// メモリ1件を削除する。Vectorize から該当 chunk のベクトルを消し、D1 からも削除する
/// (memory_chunk は ON DELETE CASCADE で連鎖削除される)。
pub(super) async fn delete_memory(mut req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let body = req.text().await?;
    let Ok(input) = serde_json::from_str::<DeleteMemory>(&body) else {
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
        .bind(&[input.memory_id.clone().into()])?
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
        .bind(&[input.memory_id.into()])?
        .run()
        .await?;

    Response::ok("Memory deleted successfully")
}

/// クエリ文の意味検索。
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
