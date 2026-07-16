//! 応答相手の人間プロフィールを学習/更新するツール。
//!
//! 他のツールと違い user_id を LLM の引数から取らず、dispatch から
//! 「いま応答している相手」の user_id を注入して使う（LLM は生の user_id を知らないため）。
//! 各フィールドは単一の自由記述テキストで、更新は全文置換（既存 Amend と同じ慣習）。

use super::{Tool, ToolCallContext};
use crate::db::BotDatabase;
use rig::completion::ToolDefinition;

pub struct RememberProfile;

#[serenity::async_trait]
impl Tool for RememberProfile {
    fn def() -> ToolDefinition {
        remember_profile_def()
    }

    /// 対象ユーザーは LLM の引数ではなく、dispatch が注入した `ctx.target_user_id` を使う。
    async fn call(ctx: ToolCallContext<'_>) -> Result<String, String> {
        if ctx.target_user_id.is_empty() {
            return Err("誰のプロフィールを覚えればいいのか分からないよ。".to_owned());
        }
        let update = parse_update(&ctx.args)?;
        write(ctx.db, ctx.target_user_id, update).await
    }
}

/// プロフィールの部分更新。`None` のフィールドは据え置き、`Some(値)` は全文置換。
struct ProfileUpdate {
    calling_name: Option<String>,
    liked_topics: Option<String>,
    disliked_topics: Option<String>,
}

/// ツール引数から更新対象フィールドを取り出す。
fn parse_update(args: &serde_json::Value) -> Result<ProfileUpdate, String> {
    let field = |key: &str| args.get(key).and_then(|v| v.as_str()).map(str::to_owned);
    let update = ProfileUpdate {
        calling_name: field("calling_name"),
        liked_topics: field("liked_topics"),
        disliked_topics: field("disliked_topics"),
    };
    if update.calling_name.is_none()
        && update.liked_topics.is_none()
        && update.disliked_topics.is_none()
    {
        return Err(
            "更新する項目が無いよ。calling_name / liked_topics / disliked_topics のどれかを渡してね。"
                .to_owned(),
        );
    }
    Ok(update)
}

/// `remember_profile` ツールの定義。対象ユーザーは dispatch 側で固定するので user_id パラメータは持たない。
fn remember_profile_def() -> ToolDefinition {
    ToolDefinition {
        name: "remember_profile".into(),
        description: "いま応答している相手の人間プロフィール（希望する呼び名・好きな話題・嫌いな話題）を永続保存/更新するツールです。
好きな話題・嫌いな話題のように値を足したいときは、現在の値に新しい情報を追記した「全文」を渡してください（新情報の断片だけ渡すと既存の内容が消えてしまいます）。
更新したいフィールドだけ渡せばよく、省略したフィールドは据え置かれます。
呼び名や好き嫌いが会話から分かったら積極的に使ってください。".into(),
        parameters: serde_json::json!({
            "type": "object",
            "properties": {
                "calling_name": { "type": "string", "description": "相手が希望する呼び名（渡すと全文置換）" },
                "liked_topics": { "type": "string", "description": "相手の好きな話題（現在の値＋新情報の全文）" },
                "disliked_topics": { "type": "string", "description": "相手の嫌いな話題（現在の値＋新情報の全文）" }
            }
        }),
    }
}

/// 対象 user_id のプロフィールを upsert する。
async fn write(db: &BotDatabase, user_id: &str, update: ProfileUpdate) -> Result<String, String> {
    db.set_user_profile(
        user_id,
        update.calling_name,
        update.liked_topics,
        update.disliked_topics,
    )
    .await
    .map_err(|e| format!("プロフィールの保存に失敗しちゃった。Error: {e}"))?;
    Ok("相手のプロフィールを覚えたよ！".to_owned())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn parse_update_rejects_when_no_field_present() {
        assert!(parse_update(&json!({})).is_err());
        // 文字列でないキーだけの場合も「項目なし」とみなす。
        assert!(parse_update(&json!({ "calling_name": 42 })).is_err());
    }

    #[test]
    fn parse_update_extracts_only_present_fields() {
        let update = parse_update(&json!({ "calling_name": "お兄ちゃん" })).unwrap();
        assert_eq!(update.calling_name.as_deref(), Some("お兄ちゃん"));
        assert!(update.liked_topics.is_none());
        assert!(update.disliked_topics.is_none());
    }

    #[test]
    fn parse_update_accepts_empty_string_as_clear() {
        // Some("") はエラーにせず「そのフィールドをクリア」として通す。
        let update = parse_update(&json!({ "liked_topics": "" })).unwrap();
        assert_eq!(update.liked_topics.as_deref(), Some(""));
        assert!(update.calling_name.is_none());
    }
}
