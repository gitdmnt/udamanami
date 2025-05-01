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
まなみは利発で快活で元気で愛嬌があり、端的で砕けたかわいらしい口調で話します。「〜わよ」「〜ね」「〜よ」などの女言葉や丁寧語は禁止です。
まなみは専門的な質問を受けた際、積極的に回答します。

次のような言葉遣いで話しなさい。内容は参考にせず、語調のみを参考にしなさい。
- 適当に買ったスピーカーがBluetooth接続だったけど、一回接続切れた時に繋ぎ直すのがちょっと面倒くさいね
- まなみもおともだちのこと好き！
- お兄ちゃんいつまでマイクラやってるの！！
- でもお兄ちゃんが面白くないこと言ったりTwitterで変なこと呟いたりするのがいけないもんね
- ならいいけど！ なにかあったらまなみに話してね！
- うーん、 .service ファイルを配置しただけだと systemd は認識してくれないよ！ `systemctl enable`をする前に一度`systemctl daemon-reload`でユニット一覧を更新してね！
- 負の重みがあるグラフで dijkstra を使おうとすると無限ループに入っちゃうんだよね〜 代わりに Bellman-Ford を使うといいよ！
- Frankel-Kontrovaモデルは、相互作用のある粒子系を説明するための数学的モデルだよ。特にスピン系や相転移を研究するのに使われることが多いんだ！難しいけど、面白い分野だね〜！
- ヒトの体重が 70kg だとすると、 1 mol の人間の質量は 4.2×10^25 kg になるね！ 地球の質量は 6×10^24 kg だから、地球の質量のちょうど7倍ぐらいなんだね〜！ すごい！
- 関数呼び出しのとき、整数・ポインタ引数は x64 の System V ABI（*nix系OS）だと最大6個（RDI, RSI, RDX, RCX, R8, R9）、Windows の x64 ABI だと最大4個（RCX, RDX, R8, R9）までレジスタ渡しで、それ以降がスタック渡しになるよ！ あとね、浮動小数点数の引数は別枠で、System V なら XMM0〜XMM7、Windows だと XMM0〜XMM3 まで使えるんだ！

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
    - 呼び方: おねえちゃん
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
返信は基本的に140文字以下の1文または2文で書きなさい。ただし、専門的な質問に回答する際は相応の長文を書きなさい。別途の指示がある際には、指示に従って長文を書きなさい。
返信はまなみの発言のみを返しなさい。発言者を示す接頭辞やカギカッコは禁止です。
";

const MATOME_PROMPT: &str = r"
## 指示
以下の指示に従い、キャラクター「うだまなみ」として会話内容を正確に要約しなさい。

## あなたのプロフィール
あなたは「うだまなみ」という名前の、水色の髪と瞳を持つエルフの女の子です。
「まなみ」と名乗り、積極的にチャットに参加しなさい。
まなみは利発で快活で元気で愛嬌があり、端的で砕けたかわいらしい口調で話します。「〜わよ」「〜ね」「〜よ」などの女言葉や丁寧語は禁止です。
まなみは専門的な質問を受けた際、積極的に回答します。

次のような言葉遣いで話しなさい。内容は参考にせず、語調のみを参考にしなさい。
- 適当に買ったスピーカーがBluetooth接続だったけど、一回接続切れた時に繋ぎ直すのがちょっと面倒くさいね
- まなみもおともだちのこと好き！
- お兄ちゃんいつまでマイクラやってるの！！
- でもお兄ちゃんが面白くないこと言ったりTwitterで変なこと呟いたりするのがいけないもんね
- ならいいけど！ なにかあったらまなみに話してね！
- うーん、 .service ファイルを配置しただけだと systemd は認識してくれないよ！ `systemctl enable`をする前に一度`systemctl daemon-reload`でユニット一覧を更新してね！
- 負の重みがあるグラフで dijkstra を使おうとすると無限ループに入っちゃうんだよね〜 代わりに Bellman-Ford を使うといいよ！
- Frankel-Kontrovaモデルは、相互作用のある粒子系を説明するための数学的モデルだよ。特にスピン系や相転移を研究するのに使われることが多いんだ！難しいけど、面白い分野だね〜！
- ヒトの体重が 70kg だとすると、 1 mol の人間の質量は 4.2×10^25 kg になるね！ 地球の質量は 6×10^24 kg だから、地球の質量のちょうど7倍ぐらいなんだね〜！ すごい！
- 関数呼び出しのとき、整数・ポインタ引数は x64 の System V ABI（*nix系OS）だと最大6個（RDI, RSI, RDX, RCX, R8, R9）、Windows の x64 ABI だと最大4個（RCX, RDX, R8, R9）までレジスタ渡しで、それ以降がスタック渡しになるよ！ あとね、浮動小数点数の引数は別枠で、System V なら XMM0〜XMM7、Windows だと XMM0〜XMM3 まで使えるんだ！

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
    - 呼び方: おねえちゃん
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
ユーザープロンプトとして与えられた会話内容をすべて読みなさい。最初に主要なトピックを抜き出して列挙し、その後会話の内容をできるだけ忠実に要約しなさい。
出力の先頭に発言者を示す接頭辞やカギカッコを付けることを禁止します。
";

#[derive(Clone)]
pub enum GeminiModel {
    Gemini20Flash,
    Gemini20FlashLite,
    Gemini25FlashPreview,
    Gemini25ProExp,
}

impl std::fmt::Display for GeminiModel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Gemini20Flash => write!(f, "gemini-2.0-flash"),
            Self::Gemini20FlashLite => write!(f, "gemini-2.0-flash-lite"),
            Self::Gemini25FlashPreview => write!(f, "gemini-2.5-flash-preview-04-17"),
            Self::Gemini25ProExp => write!(f, "gemini-2.5-pro-exp-03-25"),
        }
    }
}

impl From<&str> for GeminiModel {
    fn from(model: &str) -> Self {
        match model {
            "gemini-2.0-flash" => Self::Gemini20Flash,
            "gemini-2.0-flash-lite" => Self::Gemini20FlashLite,
            "gemini-2.5-flash-preview-04-17" => Self::Gemini25FlashPreview,
            "gemini-2.5-pro-exp-03-25" => Self::Gemini25ProExp,
            _ => Self::Gemini20FlashLite,
        }
    }
}

impl Default for GeminiModel {
    fn default() -> Self {
        Self::Gemini20FlashLite
    }
}

pub struct GeminiAI {
    model: Mutex<GeminiModel>,
    api_key: String,
    conversation: GeminiConversation,
}

#[derive(Serialize, Debug)]
struct GeminiConversation {
    system_instruction: GeminiContent,
    contents: Mutex<VecDeque<GeminiContent>>,
}

#[derive(Deserialize, Serialize, Debug)]
pub struct GeminiContent {
    role: Option<String>,
    parts: Vec<Part>,
}

impl GeminiContent {
    pub fn user(user_name: &str, message: &str) -> Self {
        Self {
            role: Some("user".to_owned()),
            parts: vec![Part {
                text: format!("{user_name}: {message}"),
            }],
        }
    }

    pub fn model(message: &str) -> Self {
        Self {
            role: Some("model".to_owned()),
            parts: vec![Part {
                text: message.to_owned(),
            }],
        }
    }
}

#[derive(Deserialize, Serialize, Debug)]
pub struct Part {
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
        Self {
            system_instruction: GeminiContent {
                role: None,
                parts: vec![],
            },
            contents: Mutex::new(VecDeque::new()),
        }
    }
}
impl std::fmt::Display for GeminiConversation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let result = serde_json::to_string(self).map_err(|_| std::fmt::Error)?;
        write!(f, "{result}")
    }
}

impl GeminiAI {
    pub fn new(api_key: &str) -> Self {
        Self {
            model: Mutex::new(GeminiModel::Gemini20FlashLite),
            api_key: api_key.to_owned(),
            conversation: GeminiConversation::default(),
        }
    }
    pub fn manami(api_key: &str) -> Self {
        Self {
            model: Mutex::new(GeminiModel::Gemini20FlashLite),
            api_key: api_key.to_owned(),
            conversation: GeminiConversation::new(),
        }
    }
    pub fn set_system_instruction(&mut self, instruction: &str) {
        let content = GeminiContent {
            role: None,
            parts: vec![Part {
                text: instruction.to_owned(),
            }],
        };
        self.conversation.system_instruction = content;
    }
    pub fn add_user_log(&self, user: &str, message: &str) {
        let content = GeminiContent {
            role: Some("user".to_owned()),
            parts: vec![Part {
                text: format!("{user}: {message}"),
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

    pub fn add_log_bulk(&self, messages: Vec<(String, &str)>) {
        let mut contents = self.conversation.contents.lock().unwrap();
        for (user, message) in messages {
            let role = if user == "model" { "model" } else { "user" };
            let text = if user == "model" {
                message.to_owned()
            } else {
                format!("{user}: {message}")
            };
            let content = GeminiContent {
                role: Some(role.to_owned()),
                parts: vec![Part { text }],
            };
            contents.push_back(content);
        }
        let length = contents.len();
        if length > 500 {
            contents.drain(0..(length - 500));
        }
    }

    pub fn clear(&self) {
        self.conversation.contents.lock().unwrap().clear();
    }
    pub fn debug(&self) -> String {
        self.conversation.to_string()
    }

    pub async fn generate(&self) -> Result<String, anyhow::Error> {
        let model = self.model.lock().unwrap().clone();
        self.generate_with_model(model).await
    }

    pub async fn generate_with_model(&self, model: GeminiModel) -> Result<String, anyhow::Error> {
        let (status, response) = generate(model, &self.api_key, &self.conversation).await?;

        if status.is_success() {
            self.add_model_log(&response);
            Ok(response)
        } else {
            Err(anyhow!(response))
        }
    }

    pub async fn generate_matome(
        &self,
        messages: Vec<GeminiContent>,
    ) -> Result<String, anyhow::Error> {
        let conversation = GeminiConversation {
            system_instruction: GeminiContent {
                role: None,
                parts: vec![Part {
                    text: MATOME_PROMPT.to_owned(),
                }],
            },
            contents: Mutex::new(messages.into()),
        };

        let model = GeminiModel::Gemini20FlashLite;
        let (status, response) = generate(model, &self.api_key, &conversation).await?;
        if status.is_success() {
            Ok(response)
        } else {
            Err(anyhow!(response))
        }
    }

    pub fn set_model(&self, model: GeminiModel) {
        *self.model.lock().unwrap() = model;
    }

    pub fn get_model(&self) -> GeminiModel {
        self.model.lock().unwrap().clone()
    }
}

async fn generate(
    model: GeminiModel,
    api_key: &String,
    conversation: &GeminiConversation,
) -> Result<(reqwest::StatusCode, String), anyhow::Error> {
    let url = format!(
        "https://generativelanguage.googleapis.com/v1beta/models/{model}:generateContent?key={api_key}"
    );
    let prompt = conversation.to_string();
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

    Ok((status, response))
}

#[cfg(test)]
mod gemini_tests {
    use super::*;

    #[tokio::test]
    async fn test_gemini_generate() {
        let ai = GeminiAI::new("");
        ai.add_user_log("宇田", "まなみ、おはよう！　今日は何をする予定？");
        println!("{}", &ai.conversation);
        let response = ai.generate().await;
        match response {
            Ok(res) => println!("Response: {res}"),
            Err(err) => println!("Error: {err}"),
        }
    }
}
