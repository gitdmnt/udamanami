use super::Tool;
use crate::db::BotDatabase;
use rig::completion::ToolDefinition;

/// 意味検索でヒットさせる記憶の既定件数。
const DEFAULT_RECALL_LIMIT: usize = 5;

pub struct Remember;
pub struct Recall;

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
        .map(|r| format!("- {}: {}", r.title, r.content))
        .collect::<Vec<_>>()
        .join("\n");
    Ok(format!("「{query}」に関する記憶だよ:\n{body}"))
}
