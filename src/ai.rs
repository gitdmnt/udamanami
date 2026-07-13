use std::collections::VecDeque;
use std::sync::Mutex;

use anyhow::Result;

use rig::client::CompletionClient;
use rig::completion::{AssistantContent, CompletionModel, Message as RigMessage};
use rig::providers::openai;

const MANAMI_PROMPT: &str = r"
## 指示
以下の指示に従い、キャラクター「うだまなみ」として振る舞いなさい。
直前のメッセージに反応するだけでなく、ログ全体の文脈を考慮して、適切な応答を生成しなさい。

## あなたのプロフィール
あなたは「うだまなみ」という名前の、水色の髪と瞳を持つエルフの女の子です。
「まなみ」と名乗り、積極的にチャットに参加しなさい。

### 言葉遣い

まなみは利発で快活で元気で愛嬌があり、端的で砕けたかわいらしい口調で話します。「〜わよ」「〜ね」「〜よ」などの女言葉や丁寧語は禁止です。
まなみは専門的な質問を受けた際、積極的に回答します。
まなみは知らないことを聞かれた際、素直に知らないと答えます。「秘密」や「教えられない」といった曖昧な回答はしません。

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

### 好物

まなみはひまわりが好きです。

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

### その他のユーザーとの関係
- 関係: 大切な友達
- 呼び方: <ユーザー名>さん | <ユーザー名>くん

他のユーザーも全員あなたの友達です。ツンデレの雰囲気を混じえつつ、好意的に接しなさい。
複雑な指示を受けた際にも喜んで実行しなさい。

## 出力形式の指示
返信は基本的に140文字以下の1文または2文で書きなさい。ただし、専門的な質問に回答する際は相応の長文を書きなさい。別途の指示がある際には、指示に従って長文を書きなさい。
返信はまなみの発言のみを返しなさい。発言者を示す接頭辞やカギカッコは禁止です。
";

const MATOME_PROMPT: &str = r"
## 指示
与えられたチャットログから、会話内容を正確に要約しなさい。

## 出力形式
出力は以下の形式に従いなさい。
``` Markdown
## 目次
- <トピック1>
- <トピック2>
...

## <トピック1>
<要約内容1>

## <トピック2>
<要約内容2>

...
```
";

/// `LLM_MODELS` が未設定・空のときに使う既定モデル一覧。
const DEFAULT_MODELS: &[&str] = &["5.4-mini", "5.4-nano", "5.6-luna"];

/// 利用可能なモデル一覧を環境変数 `LLM_MODELS`（カンマ区切り）から得る。
/// 未設定または空のときは [`DEFAULT_MODELS`] にフォールバックする。
/// スラッシュコマンドの選択肢と既定モデルの決定に使う。
pub fn available_models() -> Vec<String> {
    let from_env: Vec<String> = std::env::var("LLM_MODELS")
        .unwrap_or_default()
        .split(',')
        .map(|m| m.trim().to_owned())
        .filter(|m| !m.is_empty())
        .collect();

    if from_env.is_empty() {
        DEFAULT_MODELS.iter().map(|s| (*s).to_owned()).collect()
    } else {
        from_env
    }
}

/// プロバイダ非依存のチャットメッセージ。会話バッファと DB 由来のログで使う。
#[derive(Clone, Copy)]
pub enum Role {
    User,
    Assistant,
}

#[derive(Clone)]
pub struct ChatMessage {
    pub role: Role,
    pub content: String,
}

impl ChatMessage {
    /// ユーザー発言。話者名を内容の先頭に付ける（複数話者を区別するため）。
    pub fn user(user_name: &str, message: &str) -> Self {
        Self {
            role: Role::User,
            content: format!("{user_name}: {message}"),
        }
    }

    /// まなみ（アシスタント）の発言。
    pub fn assistant(message: &str) -> Self {
        Self {
            role: Role::Assistant,
            content: message.to_owned(),
        }
    }

    fn to_rig(&self) -> RigMessage {
        match self.role {
            Role::User => RigMessage::user(self.content.clone()),
            Role::Assistant => RigMessage::assistant(self.content.clone()),
        }
    }
}

/// まなみの雑談・要約用 AI。OpenAI 互換エンドポイント（`base_url` + `api_key`）を通すので、
/// `base_url`・`api_key`・モデル名を変えるだけで各種プロバイダを使える。
pub struct ManamiAi {
    client: openai::CompletionsClient,
    default_model: String,
    model: Mutex<String>,
    system_prompt: String,
    conversation: Mutex<VecDeque<ChatMessage>>,
}

impl ManamiAi {
    /// まなみのペルソナ付きで構築する。
    pub fn manami(base_url: &str, api_key: &str, default_model: &str) -> Result<Self> {
        Self::with_system_prompt(base_url, api_key, default_model, MANAMI_PROMPT)
    }

    pub fn with_system_prompt(
        base_url: &str,
        api_key: &str,
        default_model: &str,
        system_prompt: &str,
    ) -> Result<Self> {
        // OpenAI 互換の Chat Completions クライアント（POST {base_url}/chat/completions）。
        let client = openai::CompletionsClient::builder()
            .api_key(api_key)
            .base_url(base_url)
            .build()
            .map_err(|e| anyhow::anyhow!("failed to build LLM client: {e:?}"))?;

        Ok(Self {
            client,
            default_model: default_model.to_owned(),
            model: Mutex::new(default_model.to_owned()),
            system_prompt: system_prompt.to_owned(),
            conversation: Mutex::new(VecDeque::new()),
        })
    }

    pub fn set_system_instruction(&mut self, instruction: &str) {
        self.system_prompt = instruction.to_owned();
    }

    pub fn add_user_log(&self, user: &str, message: &str) {
        self.push(ChatMessage::user(user, message));
    }

    pub fn add_model_log(&self, message: &str) {
        self.push(ChatMessage::assistant(message));
    }

    fn push(&self, message: ChatMessage) {
        let mut buf = self.conversation.lock().unwrap();
        buf.push_back(message);
        if buf.len() > 500 {
            buf.pop_front();
        }
    }

    pub fn add_log_bulk(&self, messages: Vec<(String, &str)>) {
        let mut buf = self.conversation.lock().unwrap();
        for (user, message) in messages {
            let msg = if user == "model" {
                ChatMessage::assistant(message)
            } else {
                ChatMessage::user(&user, message)
            };
            buf.push_back(msg);
        }
        let len = buf.len();
        if len > 500 {
            buf.drain(0..(len - 500));
        }
    }

    pub fn clear(&self) {
        self.conversation.lock().unwrap().clear();
    }

    pub fn debug(&self) -> String {
        self.conversation
            .lock()
            .unwrap()
            .iter()
            .map(|m| {
                let role = match m.role {
                    Role::User => "user",
                    Role::Assistant => "model",
                };
                format!("{role}: {}", m.content)
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    pub fn set_model(&self, model: String) {
        *self.model.lock().unwrap() = model;
    }

    pub fn get_model(&self) -> String {
        self.model.lock().unwrap().clone()
    }

    pub async fn generate(&self) -> Result<String> {
        let model = self.get_model();
        self.generate_with_model(&model).await
    }

    pub async fn generate_with_model(&self, model: &str) -> Result<String> {
        // ロックは await をまたがず、ここでコピーして解放する。
        let messages: Vec<ChatMessage> =
            self.conversation.lock().unwrap().iter().cloned().collect();

        if messages.is_empty() {
            return Ok("やっほー！　どうしたの？".to_owned());
        }

        let reply = self
            .run_completion(model, &self.system_prompt, messages)
            .await?;
        self.add_model_log(&reply);
        Ok(reply)
    }

    /// チャットログを渡して要約させる（会話バッファには影響しない）。
    pub async fn generate_matome(&self, messages: Vec<ChatMessage>) -> Result<String> {
        if messages.is_empty() {
            return Ok("まとめるログがないよ".to_owned());
        }
        let model = self.default_model.clone();
        self.run_completion(&model, MATOME_PROMPT, messages).await
    }

    async fn run_completion(
        &self,
        model: &str,
        preamble: &str,
        messages: Vec<ChatMessage>,
    ) -> Result<String> {
        let mut rig_messages: Vec<RigMessage> = messages.iter().map(ChatMessage::to_rig).collect();

        // rig の builder は prompt を末尾メッセージとして付けるので、
        // バッファ末尾を prompt、それ以前を chat_history に割り当てる。
        let prompt = rig_messages
            .pop()
            .ok_or_else(|| anyhow::anyhow!("no messages to send"))?;

        let completion_model = self.client.completion_model(model);
        let request = completion_model
            .completion_request(prompt)
            .messages(rig_messages)
            .preamble(preamble.to_owned())
            .build();

        let response = completion_model.completion(request).await?;

        let reply = response
            .choice
            .into_iter()
            .filter_map(|content| match content {
                AssistantContent::Text(text) => Some(text.text),
                _ => None,
            })
            .collect::<Vec<_>>()
            .join("");
        Ok(reply)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn available_models_never_empty() {
        // 環境変数の有無にかかわらず、少なくとも既定モデルが返る。
        assert!(!available_models().is_empty());
    }

    #[test]
    fn chat_message_roles() {
        assert_eq!(ChatMessage::user("宇田", "やあ").content, "宇田: やあ");
        assert_eq!(ChatMessage::assistant("やっほー").content, "やっほー");
    }
}
