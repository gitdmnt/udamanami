pub mod shared_utils;
pub mod watch;
pub mod web_fetch_wikipedia;
pub mod web_search_wikipedia;

use rig::completion::ToolDefinition;
use serenity::async_trait;

#[async_trait]
pub trait Tool {
    fn def() -> ToolDefinition;
    async fn call(args: serde_json::Value) -> Result<String, String>;
}

/// まなみが利用できる全ツールの定義一覧。エージェント実行時にモデルへ登録する。
pub fn definitions() -> Vec<ToolDefinition> {
    vec![
        watch::Watch::def(),
        web_search_wikipedia::WebSearchWikipedia::def(),
        web_fetch_wikipedia::WebFetchWikipedia::def(),
    ]
}

/// ツール名に対応する実装を呼び出す。未知の名前はエラーを返す。
pub async fn dispatch(name: &str, args: serde_json::Value) -> Result<String, String> {
    match name {
        "watch" => watch::Watch::call(args).await,
        "web_search_wikipedia" => web_search_wikipedia::WebSearchWikipedia::call(args).await,
        "web_fetch_wikipedia" => web_fetch_wikipedia::WebFetchWikipedia::call(args).await,
        _ => Err(format!("{name}は知らないツールだよ！")),
    }
}
