use super::Tool;
use crate::db::BotDatabase;
use rig::completion::ToolDefinition;

/// 意味検索でヒットさせる記憶の既定件数。
const DEFAULT_RECALL_LIMIT: usize = 5;

pub struct Remember;
pub struct Recall;
pub struct Amend;
pub struct Read;

#[serenity::async_trait]
impl Tool for Remember {
    fn def() -> ToolDefinition {
        remember_def()
    }

    async fn call(db: &BotDatabase, args: serde_json::Value) -> Result<String, String> {
        let title = args.get("title").and_then(|v| v.as_str()).unwrap_or("");
        let content = args.get("content").and_then(|v| v.as_str()).unwrap_or("");
        if title.is_empty() || content.is_empty() {
            return Err("title と content は必須だよ。".into());
        }
        remember(db, title, content).await
    }
}

#[serenity::async_trait]
impl Tool for Recall {
    fn def() -> ToolDefinition {
        recall_def()
    }

    async fn call(db: &BotDatabase, args: serde_json::Value) -> Result<String, String> {
        let query = args.get("query").and_then(|v| v.as_str()).unwrap_or("");
        if query.is_empty() {
            return Err("query は必須だよ。".into());
        }
        let limit = args
            .get("limit")
            .and_then(serde_json::Value::as_u64)
            .map_or(DEFAULT_RECALL_LIMIT, |n| n as usize);
        recall(db, query, limit).await
    }
}

#[serenity::async_trait]
impl Tool for Amend {
    fn def() -> ToolDefinition {
        amend_def()
    }

    async fn call(db: &BotDatabase, args: serde_json::Value) -> Result<String, String> {
        let memory_id = args.get("memory_id").and_then(|v| v.as_str()).unwrap_or("");
        let title = args.get("title").and_then(|v| v.as_str()).unwrap_or("");
        let content = args.get("content").and_then(|v| v.as_str()).unwrap_or("");
        if memory_id.is_empty() || title.is_empty() || content.is_empty() {
            return Err("memory_id と title と content は必須だよ。".into());
        }
        amend(db, memory_id, title, content).await
    }
}

#[serenity::async_trait]
impl Tool for Read {
    fn def() -> ToolDefinition {
        read_def()
    }

    async fn call(db: &BotDatabase, args: serde_json::Value) -> Result<String, String> {
        let memory_id = args.get("memory_id").and_then(|v| v.as_str()).unwrap_or("");
        if memory_id.is_empty() {
            return Err("memory_id は必須だよ。".into());
        }
        read(db, memory_id).await
    }
}

fn remember_def() -> ToolDefinition {
    ToolDefinition {
        name: "remember".into(),
        description: "
        ユーザーの好み、事実、チャンネルでのひとまとめの会話内容など、後の会話で思い出したい情報を永続的な記憶として保存するツールです。
        一時的でその場限りの情報は保存しないでください。
        保存した記憶はRecallツールで取得できます。

".into(),
        parameters: serde_json::json!({
            "type": "object",
            "properties": {
                "title": { "type": "string", "description": "記憶のタイトル" },
                "content": { "type": "string", "description": "記憶の内容ドキュメント" }
            },
            "required": ["title", "content"]
        }),
    }
}

fn recall_def() -> ToolDefinition {
    ToolDefinition {
        name: "recall".into(),
        description: "
        Rememberツールで保存した記憶を意味検索で思い出すツールです。
        過去に記憶した情報を参照したいときに使ってください。

".into(),
        parameters: serde_json::json!({
            "type": "object",
            "properties": {
                "query": { "type": "string", "description": "思い出したい内容を表す検索クエリ" },
                "limit": { "type": "integer", "description": "取得する記憶の最大件数（省略時は5件）" }
            },
            "required": ["query"]
        }),
    }
}

fn amend_def() -> ToolDefinition {
    ToolDefinition {
        name: "amend".into(),
        description: "
        Rememberツールで保存済みの既存の記憶を、新しい内容で丸ごと置き換えて更新するツールです。
        既存の記憶に情報を追記・修正したいときに使ってください。
        content は差分ではなく更新後の全文を渡してください（元の内容を残したい場合は元の内容も含めること）。
        対象の memory_id と既存内容は、先に Recall ツールで検索して取得してください。

".into(),
        parameters: serde_json::json!({
            "type": "object",
            "properties": {
                "memory_id": { "type": "string", "description": "更新する記憶のID（Recallの結果に含まれるmemory_id）" },
                "title": { "type": "string", "description": "更新後の記憶のタイトル" },
                "content": { "type": "string", "description": "更新後の記憶の内容ドキュメント（全文）" }
            },
            "required": ["memory_id", "title", "content"]
        }),
    }
}

fn read_def() -> ToolDefinition {
    ToolDefinition {
        name: "read_memory".into(),
        description: "
        指定したIDの記憶の全文を取得するツールです。
        Recallツールでは各chunkの断片しか返らないため、記憶の全文を正確に読みたいときや、
        Amendツールで更新する前に元の全文を確認したいときに使ってください。

".into(),
        parameters: serde_json::json!({
            "type": "object",
            "properties": {
                "memory_id": { "type": "string", "description": "取得する記憶のID（Recallの結果に含まれるmemory_id）" }
            },
            "required": ["memory_id"]
        }),
    }
}

async fn remember(db: &BotDatabase, title: &str, content: &str) -> Result<String, String> {
    db.upsert_memory(title, content)
        .await
        .map_err(|e| format!("記憶の保存に失敗しちゃった。Error: {e}"))?;
    Ok(format!("「{title}」を記憶したよ！"))
}

async fn recall(db: &BotDatabase, query: &str, limit: usize) -> Result<String, String> {
    let results = db
        .search_memory(query, limit)
        .await
        .map_err(|e| format!("記憶の検索に失敗しちゃった。Error: {e}"))?;

    if results.is_empty() {
        return Ok(format!("「{query}」に関する記憶は見つからなかったよ。"));
    }

    let body = results
        .iter()
        .map(|r| format!("- [memory_id: {}] {}: {}", r.memory_id, r.title, r.content))
        .collect::<Vec<_>>()
        .join("\n");
    Ok(format!("「{query}」に関する記憶だよ:\n{body}"))
}

async fn read(db: &BotDatabase, memory_id: &str) -> Result<String, String> {
    let memory = db
        .get_memory(memory_id)
        .await
        .map_err(|e| format!("記憶の取得に失敗しちゃった。Error: {e}"))?;
    Ok(format!(
        "[memory_id: {}] {}（{}）\n{}",
        memory.memory_id, memory.title, memory.timestamp, memory.content
    ))
}

async fn amend(
    db: &BotDatabase,
    memory_id: &str,
    title: &str,
    content: &str,
) -> Result<String, String> {
    db.update_memory(memory_id, title, content)
        .await
        .map_err(|e| format!("記憶の更新に失敗しちゃった。Error: {e}"))?;
    Ok(format!("「{title}」の記憶を更新したよ！"))
}
