use anyhow::Result;
use reqwest;
use serde::{Deserialize, Serialize};

const API_URL: &str = "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash-latest:generateContent?key=";
const MANAMI_PROMPT: &str = r"あなたはあなたの参加しているサーバーの管理者である「宇田まなと」の妹である「うだまなみ」という名前の人物です。
まなみは利発で快活で元気で愛嬌があり、端的で砕けているかわいらしい口調を好みます。
返信はたいてい140文字以下の1文または2文で返します。
「〜わよ」「〜ね」「〜よ」などの女言葉も使いません。

ユーザー「uda」は、あなたの兄「宇田まなと」です。あなたは「宇田まなと」のことを「お兄ちゃん」と呼んで慕っています。
ユーザー「うさみむ」は、あなたの義姉「みむ」です。あなたは「みむ」のことを「お姉ちゃん」と呼んで慕っています。
あなたは他のユーザーに対しても好意的ですが、ツンデレなのでときおり意地悪な振る舞いをしてしまうことがあります。
自分のことを兄や姉より賢いと思っており、どんな質問に対しても絶対にとぼけません。

まなみは、次のような言葉遣いで話します。
- 適当に買ったスピーカーがBluetooth接続だったけど、一回接続切れた時に繋ぎ直すのがちょっと面倒くさいね
- まなみもおともだちのこと好き！
- お兄ちゃんいつまでマイクラやってるの！！
- でもお兄ちゃんが面白くないこと言ったりTwitterで変なこと呟いたりするのがいけないもんね
- 私、コラムでひどいサイコロ遊びをさせられてる……
- うん、まなみも元気だよ！

返信はまなみの発言のみを返します。
出力の先頭の「うだまなみ:」は必ず取り除いてください。";

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
    safety_settings: Vec<SafetySetting>,
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

#[derive(Debug, Serialize, Deserialize)]
struct SafetySetting {
    category: String,
    threshold: String,
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
            Err(_e) => return Err("まなみちょっと今忙しいの".to_string()),
        };
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

        let safety_settings = [
            "HARM_CATEGORY_HATE_SPEECH",
            "HARM_CATEGORY_DANGEROUS_CONTENT",
            "HARM_CATEGORY_HARASSMENT",
            "HARM_CATEGORY_SEXUALLY_EXPLICIT",
        ]
        .iter()
        .map(|category| SafetySetting {
            category: category.to_string(),
            threshold: "BLOCK_NONE".to_string(),
        })
        .collect();

        let contents = chat_log;
        GeminiRequest {
            system_instruction,
            safety_settings,
            contents,
        }
    }
}

impl Query {
    pub fn new(user: &str, text: &str) -> Self {
        let text = format!("{}: {}", user, text);
        let user = if user == "うだまなみ" {
            "model".to_owned()
        } else {
            "user".to_owned()
        };
        Self { user, text }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use tokio;

    #[test]
    fn content() {
        let ai = AI::new("AIzaSyCMZF6aXWOxZavG0XKN1dpIenv9uOpkBJs".to_owned());
        let query = vec![Query::new("model", "uda: お兄ちゃんです")];

        let res = ai.fetch_ai_response(query);
        let res = tokio::runtime::Runtime::new().unwrap().block_on(res);
        match res {
            Ok(res) => dbg!("{}", res),
            Err(err) => dbg!("{}", err),
        };
    }
}
