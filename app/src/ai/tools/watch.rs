use super::Tool;
use rig::completion::ToolDefinition;

pub struct Watch;

#[serenity::async_trait]
impl Tool for Watch {
    async fn call(_: serde_json::Value) -> Result<String, String> {
        let now = chrono::Utc::now();
        let formatted_time = now.format("%Y-%m-%d %H:%M:%S").to_string();
        Ok(format!(
            "まなみの時計によると、現在時刻は {formatted_time} だよ！"
        ))
    }

    fn def() -> ToolDefinition {
        ToolDefinition {
            name: "watch".into(),
            description: "現在時刻を確認したいときに使用できます。".into(),
            parameters: serde_json::json!({
                "type": "object",
                "properties": {}
            }),
        }
    }
}
