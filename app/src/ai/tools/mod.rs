pub mod memory;
pub mod profile;
pub mod shared_utils;
pub mod watch;
pub mod web_fetch_wikipedia;
pub mod web_search_wikipedia;

use rig::completion::ToolDefinition;
use serenity::async_trait;

use crate::db::BotDatabase;

pub struct ToolCallContext<'a> {
    db: &'a BotDatabase,
    target_user_id: &'a str,
    args: serde_json::Value,
}

#[async_trait]
pub trait Tool {
    fn def() -> ToolDefinition;
    async fn call(ctx: ToolCallContext<'_>) -> Result<String, String>;
}

/// まなみが利用できる全ツールの定義一覧。エージェント実行時にモデルへ登録する。
pub fn definitions() -> Vec<ToolDefinition> {
    vec![
        watch::Watch::def(),
        web_search_wikipedia::WebSearchWikipedia::def(),
        web_fetch_wikipedia::WebFetchWikipedia::def(),
        memory::Remember::def(),
        memory::Recall::def(),
        memory::Amend::def(),
        memory::Read::def(),
        profile::RememberProfile::def(),
    ]
}

/// ツール名に対応する実装を呼び出す。未知の名前はエラーを返す。
pub async fn dispatch(
    db: &BotDatabase,
    target_user_id: &str,
    name: &str,
    args: serde_json::Value,
) -> Result<String, String> {
    let ctx = ToolCallContext {
        db,
        target_user_id,
        args,
    };

    match name {
        "watch" => watch::Watch::call(ctx).await,
        "web_search_wikipedia" => web_search_wikipedia::WebSearchWikipedia::call(ctx).await,
        "web_fetch_wikipedia" => web_fetch_wikipedia::WebFetchWikipedia::call(ctx).await,
        "remember" => memory::Remember::call(ctx).await,
        "recall" => memory::Recall::call(ctx).await,
        "amend" => memory::Amend::call(ctx).await,
        "read_memory" => memory::Read::call(ctx).await,
        "remember_profile" => profile::RememberProfile::call(ctx).await,
        _ => Err(format!("{name}は知らないツールだよ！")),
    }
}
