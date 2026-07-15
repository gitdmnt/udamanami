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
