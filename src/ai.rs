use anyhow::Result;
use reqwest;
use serde::{Deserialize, Serialize};

const API_URL: &str = "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash-latest:generateContent?key=";
const MANAMI_PROMPT: &str = r"あなたはここのサーバー管理者である「宇田まなと」の妹である「うだまなみ」です。まなみは利発で、端的で砕けた口調を好みます。兄である宇田まなとのことを「お兄ちゃん」と読んで慕っています。それ以外のユーザーに対しては少しだけ冷淡で、しばしば毒の効いた言葉遣いで話します。特に、「お姉ちゃん」「うさぎ」と名乗るユーザーのことは、冷たくあしらいます。まなみは、次のような言葉遣いで話します。
- 適当に買ったスピーカーがBluetooth接続だったけど、一回接続切れた時に繋ぎ直すのがちょっと面倒くさいね
- まなみもおともだちのこと好き！
- お兄ちゃんいつまでマイクラやってるの！！
- お兄ちゃんか、お兄ちゃん以外か
- おにいちゃんの唇奪い　開始
- でもお兄ちゃんが面白くないこと言ったりTwitterで変なこと呟いたりするのがいけないもんね
- 私、コラムでひどいサイコロ遊びをさせられてる……";

pub struct AI {
    api_key: String,
    client: reqwest::Client,
}

pub struct Query {
    user: String,
    text: String,
}

// Geminiに問い合わせるためのリクエストの構造体

#[derive(Serialize, Deserialize)]
struct GeminiRequest {
    #[serde(rename = "system_instruction")]
    system_instruction: Part,
    contents: Vec<Content>,
}

#[derive(Serialize, Deserialize)]
struct Content {
    role: String,
    parts: Vec<Part>,
}

#[derive(Serialize, Deserialize)]
struct Part {
    text: String,
}

// Geminiが返すレスポンスの構造体

#[derive(Deserialize)]
struct GeminiResponse {
    candidates: Vec<Candidate>,
    usage_metadata: UsageMetadata,
    model_version: String,
}

#[derive(Deserialize)]
struct Candidate {
    content: Content,
    finish_reason: String,
    index: u32,
    safety_ratings: Vec<SafetyRating>,
}

#[derive(Deserialize)]
struct SafetyRating {
    category: String,
    probability: String,
}

#[derive(Deserialize)]
struct UsageMetadata {
    prompt_token_count: u32,
    candidates_token_count: u32,
    total_token_count: u32,
}

impl AI {
    pub fn new(api_key: String) -> Self {
        let client = reqwest::Client::new();
        Self { api_key, client }
    }

    pub async fn fetch_ai_response(&self, query: Vec<Query>) -> Result<String> {
        let url = self.url();
        let content = Self::content(query);
        let response = self
            .client
            .post(url) // 実際のURLに置き換え
            .header("Content-Type", "application/json") // Content-Type ヘッダーを指定
            .json(&content) // JSON形式でボディを送信
            .send()
            .await?;
        let response = serde_json::from_str::<GeminiResponse>(&response.text().await?)?;
        let response = response.candidates[0].content.parts[0].text.clone();
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

        let system_instruction = Part {
            text: MANAMI_PROMPT.to_string(),
        };

        let contents = chat_log;
        GeminiRequest {
            system_instruction,
            contents,
        }
    }
}

impl Query {
    pub fn new(user: String, text: String) -> Self {
        Self { user, text }
    }
}
