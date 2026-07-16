use super::Tool;
use crate::ai::tools::shared_utils::html_to_text;
use rig::completion::ToolDefinition;

pub struct WebFetchWikipedia;

#[serenity::async_trait]
impl Tool for WebFetchWikipedia {
    fn def() -> ToolDefinition {
        def()
    }

    async fn call(_: &crate::db::BotDatabase, args: serde_json::Value) -> Result<String, String> {
        let title = args.get("title").and_then(|v| v.as_str()).unwrap_or("");
        let lang = args.get("lang").and_then(|v| v.as_str()).unwrap_or("ja");
        fetch(title, lang).await
    }
}

async fn fetch(title: &str, lang: &str) -> Result<String, String> {
    let url = format!(
            "https://{lang}.wikipedia.org/w/api.php?action=query&prop=extracts&titles={title}&format=json"
        );
    let client = reqwest::Client::builder()
        .user_agent(
            "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:152.0) Gecko/20100101 Firefox/152.0",
        )
        .build()
        .map_err(|e| format!("Wikipedia にアクセスできなかったよ。Error: {}", e))?;
    let resp = client
        .get(&url)
        .send()
        .await
        .map_err(|e| format!("Wikipedia にアクセスできなかったよ。Error: {}", e))?;
    let json: serde_json::Value = resp
        .json()
        .await
        .map_err(|e| format!("Wikipedia のレスポンスを解析できなかったよ。Error: {}", e))?;
    let pages = json["query"]["pages"]
        .as_object()
        .ok_or("Wikipedia のレスポンスが不正だよ。")?;
    if pages.is_empty() {
        return Err(format!(
            "Wikipedia に「{title}」というページは存在しないよ。"
        ));
    }
    let page = pages
        .values()
        .next()
        .ok_or("Wikipedia のレスポンスが不正だよ。")?;
    let extract = page["extract"].as_str().unwrap_or("");
    if extract.is_empty() {
        return Err(format!(
            "Wikipedia では「{title}」の内容が見つからなかったよ。"
        ));
    }

    Ok(format!(
        "Wikipedia の「{title}」の内容だよ:\n{}",
        html_to_text(extract)
    ))
}

fn def() -> super::ToolDefinition {
    super::ToolDefinition {
        name: "web_fetch_wikipedia".into(),
        description: "Wikipedia から指定ページの内容を取得するツール".into(),
        parameters: serde_json::json!({
            "type": "object",
            "properties": {
                "title": { "type": "string", "description": "ページタイトル" },
                "lang": { "type": "string", "description": "言語コード（例: 'ja', 'en'）" }
            },
            "required": ["title", "lang"]
        }),
    }
}
