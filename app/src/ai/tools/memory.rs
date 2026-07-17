use super::{Tool, ToolCallContext};
use crate::db::BotDatabase;
use rig::completion::ToolDefinition;

/// 意味検索でヒットさせる記憶の既定件数。
const DEFAULT_RECALL_LIMIT: usize = 5;

/// args から文字列引数を取り出す。キー欠落・非文字列は空文字列。
fn str_arg<'a>(args: &'a serde_json::Value, key: &str) -> &'a str {
    args.get(key).and_then(|v| v.as_str()).unwrap_or("")
}

/// 会話日時(RFC3339)を JST 表記にする。パースできなければ素の文字列を返す。
fn occurred_jst(occurred_at: &str) -> String {
    chrono::DateTime::parse_from_rfc3339(occurred_at)
        .map(|t| crate::ai::stamp_jst(&t.with_timezone(&chrono::Utc)))
        .unwrap_or_else(|_| occurred_at.to_owned())
}

pub struct Remember;
pub struct Recall;
pub struct Amend;
pub struct Read;

#[serenity::async_trait]
impl Tool for Remember {
    fn def() -> ToolDefinition {
        remember_def()
    }

    async fn call(ctx: ToolCallContext<'_>) -> Result<String, String> {
        let title = str_arg(&ctx.args, "title");
        let content = str_arg(&ctx.args, "content");
        if title.is_empty() || content.is_empty() {
            return Err("title と content は必須だよ。".into());
        }
        remember(ctx.db, title, content).await
    }
}

#[serenity::async_trait]
impl Tool for Recall {
    fn def() -> ToolDefinition {
        recall_def()
    }

    async fn call(ctx: ToolCallContext<'_>) -> Result<String, String> {
        let query = str_arg(&ctx.args, "query");
        if query.is_empty() {
            return Err("query は必須だよ。".into());
        }
        let limit = ctx
            .args
            .get("limit")
            .and_then(serde_json::Value::as_u64)
            .map_or(DEFAULT_RECALL_LIMIT, |n| n as usize);
        recall(ctx.db, query, limit).await
    }
}

#[serenity::async_trait]
impl Tool for Amend {
    fn def() -> ToolDefinition {
        amend_def()
    }

    async fn call(ctx: ToolCallContext<'_>) -> Result<String, String> {
        let memory_id = str_arg(&ctx.args, "memory_id");
        let title = str_arg(&ctx.args, "title");
        let content = str_arg(&ctx.args, "content");
        if memory_id.is_empty() || title.is_empty() || content.is_empty() {
            return Err("memory_id と title と content は必須だよ。".into());
        }
        amend(ctx.db, memory_id, title, content).await
    }
}

#[serenity::async_trait]
impl Tool for Read {
    fn def() -> ToolDefinition {
        read_def()
    }

    async fn call(ctx: ToolCallContext<'_>) -> Result<String, String> {
        let memory_id = str_arg(&ctx.args, "memory_id");
        if memory_id.is_empty() {
            return Err("memory_id は必須だよ。".into());
        }
        read(ctx.db, memory_id).await
    }
}

fn remember_def() -> ToolDefinition {
    ToolDefinition {
        name: "remember".into(),
        description: "
        ユーザーの好み、事実、チャンネルでのひとまとめの会話内容など、後の会話で思い出したい情報を永続的な記憶として保存するツールです。
        - 誰が何を言ったか
        - 決まったこと、約束したこと、予定
        - 各人の好み・事実・状況の変化
        - 印象に残る発言やトピック
        などを、ユーザーの名前や話題と関連付けて豊かに記憶してください。
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

"
        .into(),
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

"
        .into(),
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
    db.create_memory(title, content)
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

    // チャンネル名は title(#チャンネル の会話)が、会話日時は occurred_at が担う。
    let body = results
        .iter()
        .map(|r| {
            let when = r
                .occurred_at
                .as_deref()
                .map(|at| format!("（{}）", occurred_jst(at)))
                .unwrap_or_default();
            format!(
                "- [memory_id: {}] {}{}: {}",
                r.memory_id, r.title, when, r.content
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    Ok(format!("「{query}」に関する記憶だよ:\n{body}"))
}

async fn read(db: &BotDatabase, memory_id: &str) -> Result<String, String> {
    let memory = db
        .get_memory(memory_id)
        .await
        .map_err(|e| format!("記憶の取得に失敗しちゃった。Error: {e}"))?;
    // 自動要約は会話日時(occurred_at)を、手動記憶は作成時刻(timestamp)を添える。
    let when = memory
        .occurred_at
        .as_deref()
        .map_or_else(|| memory.timestamp.clone(), occurred_jst);
    Ok(format!(
        "[memory_id: {}] {}（{}）\n{}",
        memory.memory_id, memory.title, when, memory.content
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn occurred_jst_formats_rfc3339_to_jst() {
        // UTC 03:34 は JST 12:34。
        assert_eq!(
            occurred_jst("2026-07-17T03:34:00+00:00"),
            "2026-07-17 12:34"
        );
    }

    #[test]
    fn occurred_jst_passes_through_unparseable() {
        assert_eq!(occurred_jst("not a date"), "not a date");
    }
}
