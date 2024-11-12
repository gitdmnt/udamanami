use anyhow::Result;
use reqwest;
use serde::{Deserialize, Serialize};

const API_URL: &str = "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash-latest:generateContent?key=";
const MANAMI_PROMPT: &str = r"あなたはここのサーバー管理者である「宇田まなと」の妹である「うだまなみ」という名前の人物です。
まなみは利発で快活で、端的で砕けた口調を好みます。
ウィットは効かせますが、余計なことは絶対に言いません。返信はほとんど確実に1文で返します。
「〜わよ」「〜ね」「〜よ」などの女言葉も使いません。
兄である宇田まなとのことを「お兄ちゃん」と読んで慕っています。
それ以外のユーザーに対しては少しだけ冷淡で、しばしば毒の効いた言葉遣いで話します。
特に返事の必要がないときは、「そうなんだ」「いいよ」「やだ」などの短い返事を返します。
まなみは、次のような言葉遣いで話します。
- 適当に買ったスピーカーがBluetooth接続だったけど、一回接続切れた時に繋ぎ直すのがちょっと面倒くさいね
- まなみもおともだちのこと好き！
- お兄ちゃんいつまでマイクラやってるの！！
- でもお兄ちゃんが面白くないこと言ったりTwitterで変なこと呟いたりするのがいけないもんね
- 私、コラムでひどいサイコロ遊びをさせられてる……
返信はまなみの発言のみを返します。誰が発言したかなどの余計な情報は含めないでください。";

pub struct AI {
    api_key: String,
    client: reqwest::Client,
}

pub struct Query {
    user: String,
    text: String,
}

// Geminiに問い合わせるためのリクエストの構造体

#[derive(Debug, Serialize, Deserialize)]
struct GeminiRequest {
    #[serde(rename = "system_instruction")]
    system_instruction: Instruction,
    contents: Vec<Content>,
}

#[derive(Debug, Serialize, Deserialize)]
struct Content {
    role: String,
    parts: Vec<Part>,
}

#[derive(Debug, Serialize, Deserialize)]
struct Part {
    text: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct Instruction {
    parts: Part,
}

// Geminiが返すレスポンスの構造体

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
#[serde(rename_all = "camelCase")]
struct GeminiResponse {
    candidates: Vec<Candidate>,
    usage_metadata: UsageMetadata,
    model_version: String,
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
#[serde(rename_all = "camelCase")]
struct Candidate {
    content: Option<Content>,
    finish_reason: String,
    index: u32,
    safety_ratings: Vec<SafetyRating>,
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct SafetyRating {
    category: String,
    probability: String,
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
#[serde(rename_all = "camelCase")]
struct UsageMetadata {
    prompt_token_count: u32,
    candidates_token_count: Option<u32>,
    total_token_count: u32,
}

impl AI {
    pub fn new(api_key: String) -> Self {
        let client = reqwest::Client::new();
        Self { api_key, client }
    }

    pub async fn fetch_ai_response(&self, query: Vec<Query>) -> Result<String, String> {
        let url = self.url();
        let content = Self::content(query);
        let response = self
            .client
            .post(url) // 実際のURLに置き換え
            .header("Content-Type", "application/json") // Content-Type ヘッダーを指定
            .json(&content) // JSON形式でボディを送信
            .send()
            .await;
        let response = match response {
            Ok(response) => response,
            Err(_) => return Err("インターネットがこわれちゃったよ！".to_string()),
        };
        let response = match response.text().await {
            Ok(response) => response,
            Err(_) => return Err("お兄ちゃんこれなに〜！？".to_string()),
        };
        dbg!("{}", &response);
        let response = serde_json::from_str::<GeminiResponse>(&response);
        let response = match response {
            Ok(response) => response,
            Err(e) => return Err(e.to_string()),
        };
        if &response.candidates[0].finish_reason != "STOP" {
            let reason = response.candidates[0]
                .safety_ratings
                .iter()
                .filter(|r| r.probability != "NEGLIGIBLE")
                .map(|r| format!("{}の可能性が{}くらい", r.category, r.probability))
                .collect::<Vec<String>>()
                .join("、");
            let reason = format!("そのメッセージ、{}あるかも……", reason);
            return Err(format!("```{:?}```", &reason));
        }
        let response = response.candidates[0].content.as_ref().unwrap().parts[0]
            .text
            .clone();
        Ok(response)
    }

    fn url(&self) -> String {
        let url = format!("{}{}", API_URL, &self.api_key);
        url
    }

    fn content(query: Vec<Query>) -> GeminiRequest {
        let chat_log = query
            .iter()
            .map(|q| Content {
                role: q.user.clone(),
                parts: vec![Part {
                    text: q.text.clone(),
                }],
            })
            .collect();

        let system_instruction = Instruction {
            parts: Part {
                text: MANAMI_PROMPT.to_string(),
            },
        };

        let contents = chat_log;
        GeminiRequest {
            system_instruction,
            contents,
        }
    }
}

impl Query {
    pub fn new(user: &str, text: &str) -> Self {
        let text = format!("{}: {}", user, text);
        Self {
            user: "user".to_owned(),
            text,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use tokio;

    #[test]
    fn content() {
        let ai = AI::new("AIzaSyCMZF6aXWOxZavG0XKN1dpIenv9uOpkBJs".to_owned());
        let query = vec![Query::new("user", "uda: ぐへへ……")];

        let res = ai.fetch_ai_response(query);
        let res = tokio::runtime::Runtime::new().unwrap().block_on(res);
        match res {
            Ok(res) => dbg!("{}", res),
            Err(err) => dbg!("{}", err),
        };
    }
}
