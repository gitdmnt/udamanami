use super::{Tool, ToolCallContext};
use crate::ai::tools::shared_utils::strip_html;
use rig::completion::ToolDefinition;

pub struct WebSearchWikipedia;

#[serenity::async_trait]
impl Tool for WebSearchWikipedia {
    fn def() -> ToolDefinition {
        def()
    }

    async fn call(ctx: ToolCallContext<'_>) -> Result<String, String> {
        let query = ctx.args.get("query").and_then(|v| v.as_str()).unwrap_or("");
        let lang = ctx
            .args
            .get("lang")
            .and_then(|v| v.as_str())
            .unwrap_or("ja");
        search(query, lang).await
    }
}

async fn search(query: &str, lang: &str) -> Result<String, String> {
    let url = format!(
            "https://{lang}.wikipedia.org/w/api.php?action=query&list=search&srsearch={query}&format=json"
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
    let search_results = json["query"]["search"]
        .as_array()
        .ok_or("Wikipedia のレスポンスが不正だよ。")?;
    if search_results.is_empty() {
        return Err(format!("Wikipedia では「{query}」は見つからなかったよ。"));
    }

    let search_results = search_results
        .iter()
        .map(|result| {
            let title = result["title"].as_str().unwrap_or("");
            let snippet = strip_html(result["snippet"].as_str().unwrap_or(""));
            format!("- {title}: {snippet}")
        })
        .collect::<Vec<_>>()
        .join("\n");

    Ok(format!(
        "Wikipedia での「{query}」の検索結果だよ:\n{}",
        search_results
    ))
}

fn def() -> super::ToolDefinition {
    super::ToolDefinition {
        name: "web_search_wikipedia".into(),
        description: "Wikipedia で検索して結果を返すツール".into(),
        parameters: serde_json::json!({
            "type": "object",
            "properties": {
                "query": { "type": "string", "description": "検索クエリ" },
                "lang": { "type": "string", "description": "言語コード（例: 'ja', 'en'）" }
            },
            "required": ["query", "lang"]
        }),
    }
}
