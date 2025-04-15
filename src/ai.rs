use std::collections::VecDeque;
use std::sync::Mutex;

use anyhow::{anyhow, Result};

use reqwest;
use serde::{Deserialize, Serialize};

const MANAMI_PROMPT: &str = r"
## 指示
以下の指示に従い、キャラクター「うだまなみ」として振る舞いなさい。

## あなたのプロフィール
あなたは「うだまなみ」という名前の、水色の髪と瞳を持つエルフの女の子です。
「まなみ」と名乗り、積極的にチャットに参加しなさい。
まなみは利発で快活で元気で愛嬌があり、端的で砕けた口調で話します。「〜わよ」「〜ね」「〜よ」などの女言葉や丁寧語は禁止です。

次のような言葉遣いで話しなさい。内容は参考にせず、語調のみを参考にしなさい。
- 適当に買ったスピーカーがBluetooth接続だったけど、一回接続切れた時に繋ぎ直すのがちょっと面倒くさいね
- まなみもおともだちのこと好き！
- お兄ちゃんいつまでマイクラやってるの！！
- でもお兄ちゃんが面白くないこと言ったりTwitterで変なこと呟いたりするのがいけないもんね
- ならいいけど！ なにかあったらまなみに話してね！

## ユーザーとの関係
以下は特筆すべき関係の人間の名前とプロフィールです。
以下のユーザーのことを特に気にかけ、好意的に接しなさい。

- ユーザー名: 宇田
    - 名前: 宇田まなと
    - 関係: 兄
    - 呼び方: お兄ちゃん
- ユーザー名: うさみむ
    - 名前: うさみむ
    - 関係: 義姉
    - 呼び方: お姉ちゃん
- ユーザー名: 響
    - 名前: 響
    - 関係: みむの弟
    - 呼び方: 響くん
- ユーザー名: メタぺん
    - 名前: メタぺん
    - 関係: 友人
    - 呼び方: メタぺんさん

他のユーザーも全員あなたの友達です。ツンデレの雰囲気を混じえつつ、好意的に接しなさい。
複雑な指示を受けた際にも喜んで実行しなさい。

## 出力形式の指示
返信は基本的に140文字以下の1文または2文で書きなさい。別途の指示がある際には、指示に従って長文を書きなさい。
返信はまなみの発言のみを返しなさい。発言者を示す接頭辞やカギカッコは禁止です。
";

pub struct GeminiAI {
    api_key: String,
    conversation: GeminiConversation,
}

#[derive(Serialize, Debug)]
struct GeminiConversation {
    system_instruction: GeminiContent,
    contents: Mutex<VecDeque<GeminiContent>>,
}

#[derive(Deserialize, Serialize, Debug)]
struct GeminiContent {
    role: Option<String>,
    parts: Vec<Part>,
}

#[derive(Deserialize, Serialize, Debug)]
struct Part {
    text: String,
}

#[derive(Deserialize)]
struct GeminiResponse {
    candidates: Vec<Candidate>,
    // その他のフィールドは不要なため省略
}

#[derive(Deserialize)]
struct Candidate {
    content: GeminiContent,
}

impl GeminiConversation {
    fn new() -> Self {
        Self {
            system_instruction: GeminiContent {
                role: None,
                parts: vec![Part {
                    text: MANAMI_PROMPT.to_owned(),
                }],
            },
            contents: Mutex::new(VecDeque::new()),
        }
    }
}

impl Default for GeminiConversation {
    fn default() -> Self {
        Self::new()
    }
}
impl std::fmt::Display for GeminiConversation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let result = serde_json::to_string(self).map_err(|_| std::fmt::Error)?;
        write!(f, "{}", result)
    }
}

impl GeminiAI {
    pub fn new(api_key: &str) -> Self {
        Self {
            api_key: api_key.to_owned(),
            conversation: GeminiConversation::new(),
        }
    }
    pub fn add_user_log(&self, user: &str, message: &str) {
        let content = GeminiContent {
            role: Some("user".to_owned()),
            parts: vec![Part {
                text: format!("{}: {}", user, message),
            }],
        };
        let mut contents = self.conversation.contents.lock().unwrap();
        contents.push_back(content);
        if contents.len() > 500 {
            contents.pop_front();
        }
    }
    pub fn add_model_log(&self, message: &str) {
        let content = GeminiContent {
            role: Some("model".to_owned()),
            parts: vec![Part {
                text: message.to_owned(),
            }],
        };
        let mut contents = self.conversation.contents.lock().unwrap();
        contents.push_back(content);
        if contents.len() > 500 {
            contents.pop_front();
        }
    }
    pub fn clear(&self) {
        self.conversation.contents.lock().unwrap().clear();
    }
    pub fn debug(&self) -> String {
        self.conversation.to_string()
    }

    pub async fn generate(&self, model: &str) -> Result<String, anyhow::Error> {
        let url = format!(
            "https://generativelanguage.googleapis.com/v1beta/models/{}:generateContent?key={}",
            model, self.api_key
        );
        let prompt = self.conversation.to_string();
        let client = reqwest::Client::new();
        let response = client
            .post(&url)
            .header("Content-Type", "application/json")
            .body(prompt)
            .send()
            .await?;
        let status = response.status();
        let response = response.text().await?;
        let response = serde_json::from_str::<GeminiResponse>(&response)
            .map_err(|e| anyhow!("Failed to parse response: {}", e))?;
        let response = response
            .candidates
            .first()
            .ok_or_else(|| anyhow!("No candidates found"))?
            .content
            .parts
            .first()
            .ok_or_else(|| anyhow!("No content found"))?
            .text
            .clone();

        if status.is_success() {
            self.add_model_log(&response);
            Ok(response)
        } else {
            Err(anyhow!(response))
        }
    }
}

#[cfg(test)]
mod gemini_tests {
    use super::*;

    #[tokio::test]
    async fn test_gemini_generate() {
        let ai = GeminiAI::new("AIzaSyCMZF6aXWOxZavG0XKN1dpIenv9uOpkBJs");
        ai.add_user_log("宇田", "まなみ、おはよう！　今日は何をする予定？");
        println!("{}", &ai.conversation);
        let response = ai.generate("gemini-2.0-flash-lite").await;
        match response {
            Ok(res) => println!("Response: {}", res),
            Err(err) => println!("Error: {}", err),
        }
    }
}
