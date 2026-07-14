use std::collections::{BTreeSet, VecDeque};
use std::sync::Mutex;

use anyhow::Result;
use serde_json::json;

use rig::agent::run::{AgentRun, AgentRunStep, ModelTurn, ModelTurnOutcome};
use rig::client::CompletionClient;
use rig::completion::{AssistantContent, CompletionModel, Message as RigMessage, ToolDefinition};
use rig::message::{ToolResultContent, UserContent}; // ← 実際の module 位置はコンパイラに従って調整
use rig::providers::openai;
use rig::OneOrMany;

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
const DEFAULT_MODELS: &[&str] = &["gpt-5.4-mini", "gpt-5.4-nano", "gpt-5.6-luna"];

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
#[derive(Clone)]
pub enum Role {
    User { name: String },
    Assistant,
}

#[derive(Clone)]
pub struct ChatMessage {
    pub role: Role,
    pub content: String,
}

impl ChatMessage {
    /// ユーザー発言。。
    pub fn user(user_name: &str, message: &str) -> Self {
        Self {
            role: Role::User {
                name: user_name.to_owned(),
            },
            content: message.to_owned(),
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
        match &self.role {
            Role::User { name } => RigMessage::user(format!("{}: {}", name, self.content)),
            Role::Assistant => RigMessage::assistant(self.content.clone()),
        }
    }
}

/// まなみの雑談・要約用 AI。OpenAI 互換エンドポイント（`base_url` + `api_key`）を通すので、
/// `base_url`・`api_key`・モデル名を変えるだけで各種プロバイダを使える。
pub struct ManamiAi {
    client: openai::Client,
    default_model: String,
    model: Mutex<String>,
    effort: Mutex<String>,
    system_prompt: String,
    conversation: Mutex<VecDeque<ChatMessage>>,
}

impl ManamiAi {
    /// まなみのペルソナ付きでチャットボットのクライアントを構築する。
    pub fn manami(
        base_url: &str,
        api_key: &str,
        default_model: &str,
        default_effort: &str,
    ) -> Result<Self> {
        Self::with_system_prompt(
            base_url,
            api_key,
            default_model,
            default_effort,
            MANAMI_PROMPT,
        )
    }

    /// システムプロンプトを指定してチャットボットのクライアントを構築する。
    pub fn with_system_prompt(
        base_url: &str,
        api_key: &str,
        default_model: &str,
        default_effort: &str,
        system_prompt: &str,
    ) -> Result<Self> {
        // OpenAI 互換の Responses API クライアント（POST {base_url}/responses）。
        let client = openai::Client::builder()
            .api_key(api_key)
            .base_url(base_url)
            .build()
            .map_err(|e| anyhow::anyhow!("failed to build LLM client: {e:?}"))?;

        Ok(Self {
            client,
            default_model: default_model.to_owned(),
            model: Mutex::new(default_model.to_owned()),
            effort: Mutex::new(default_effort.to_owned()),
            system_prompt: system_prompt.to_owned(),
            conversation: Mutex::new(VecDeque::new()),
        })
    }

    /// システムプロンプトを変更する。会話バッファはクリアされない。
    pub fn set_system_instruction(&mut self, instruction: &str) {
        self.system_prompt = instruction.to_owned();
    }

    /// 会話バッファにユーザー発言を追加する。話者名は Role に保持し、
    /// LLM 送信時（to_rig）に本文の先頭へ付与する。
    pub fn add_user_log(&self, user: &str, message: &str) {
        self.push(ChatMessage::user(user, message));
    }

    /// 会話バッファにまなみの発言を追加する。
    pub fn add_model_log(&self, message: &str) {
        self.push(ChatMessage::assistant(message));
    }

    /// 会話バッファにメッセージを追加する。最大 500 件まで保持する。
    fn push(&self, message: ChatMessage) {
        let mut buf = self.conversation.lock().unwrap();
        buf.push_back(message);
        if buf.len() > 500 {
            buf.pop_front();
        }
    }

    /// 会話バッファに複数のメッセージを追加する。最大 500 件まで保持する。
    pub fn add_log_bulk(&self, messages: Vec<(Role, &str)>) {
        let mut buf = self.conversation.lock().unwrap();
        for (role, message) in messages {
            let msg = match role {
                Role::User { name } => ChatMessage::user(&name, message),
                Role::Assistant => ChatMessage::assistant(message),
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
                let role = match &m.role {
                    Role::User { name } => format!("user ({})", name),
                    Role::Assistant => "model".into(),
                };
                format!("{role}: {}", m.content)
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// 現在のモデルを変更する。会話バッファはクリアされない。
    pub fn set_model(&self, model: String) {
        *self.model.lock().unwrap() = model;
    }

    /// 現在のモデルを取得する。
    pub fn get_model(&self) -> String {
        self.model.lock().unwrap().clone()
    }

    /// 現在のreasoning effortを変更する。会話バッファはクリアされない。
    pub fn set_effort(&self, effort: String) {
        *self.effort.lock().unwrap() = effort;
    }

    /// 現在のreasoning effortを取得する。
    pub fn get_effort(&self) -> String {
        self.effort.lock().unwrap().clone()
    }

    /// 現在のモデルを使ってメッセージを生成する。
    pub async fn generate(&self) -> Result<String> {
        let model = self.get_model();
        self.generate_with_model(&model).await
    }

    /// 現在のモデルを使ってメッセージを生成する。モデル名が空文字列なら現在のモデルにフォールバックする。
    pub async fn generate_with_model(&self, model: &str) -> Result<String> {
        // 全レスモード未設定時などモデルが空なら、現在のモデルにフォールバックする。
        let model = if model.trim().is_empty() {
            self.get_model()
        } else {
            model.to_owned()
        };

        let effort = self.effort.lock().unwrap().clone();

        // ロックは await をまたがず、ここでコピーして解放する。
        let messages: Vec<ChatMessage> =
            self.conversation.lock().unwrap().iter().cloned().collect();

        if messages.is_empty() {
            return Ok("やっほー！　どうしたの？".to_owned());
        }

        let reply = self.run_agent(&model, &effort, messages).await?;
        self.add_model_log(&reply);
        Ok(reply)
    }

    /// チャットログを渡して要約させる。
    pub async fn generate_matome(&self, messages: Vec<ChatMessage>) -> Result<String> {
        if messages.is_empty() {
            return Ok("まとめるログがないよ".to_owned());
        }
        let model = self.default_model.clone();
        let effort = "low";
        self.run_completion(&model, effort, MATOME_PROMPT, messages)
            .await
    }

    fn tool_definitions(&self) -> Vec<ToolDefinition> {
        vec![
            tools::watch_definition(),
            tools::web_search_wikipedia_definition(),
            tools::web_fetch_wikipedia_definition(),
        ]
    }

    async fn dispatch_tool(&self, name: &str, args: serde_json::Value) -> String {
        match name {
            "watch" => tools::watch(),
            "web_search_wikipedia" => {
                let query = args.get("query").and_then(|v| v.as_str()).unwrap_or("");
                let lang = args.get("lang").and_then(|v| v.as_str()).unwrap_or("ja");
                match tools::web_search_wikipedia(query, lang).await {
                    Ok(result) => result,
                    Err(err) => err,
                }
            }
            "web_fetch_wikipedia" => {
                let title = args.get("title").and_then(|v| v.as_str()).unwrap_or("");
                let lang = args.get("lang").and_then(|v| v.as_str()).unwrap_or("ja");
                match tools::web_fetch_wikipedia(title, lang).await {
                    Ok(result) => result,
                    Err(err) => err,
                }
            }
            _ => format!("{name}は知らないツールだよ！"),
        }
    }

    /// 内部用。Rig の AgentRun を使って、ツール呼び出しを含む複数ターンの会話を進める。
    async fn run_agent(
        &self,
        model: &str,
        effort: &str,
        messages: Vec<ChatMessage>,
    ) -> anyhow::Result<String> {
        let history: Vec<RigMessage> = messages.iter().map(ChatMessage::to_rig).collect();
        let prompt = history
            .last()
            .ok_or_else(|| anyhow::anyhow!("no messages to send"))?
            .clone();

        let tool_defs = self.tool_definitions();
        let tool_names: BTreeSet<String> = tool_defs
            .iter()
            .map(|t: &ToolDefinition| t.name.clone())
            .collect();

        let mut run = AgentRun::new(prompt).with_history(history).max_turns(10);

        let mut response_blocks: Vec<Block> = Vec::new(); // まなみの reasoning 表示用

        loop {
            match run.next_step()? {
                // LLMによる応答
                AgentRunStep::CallModel {
                    prompt, history, ..
                } => {
                    let cm = self.client.completion_model(model);
                    let request = cm
                        .completion_request(prompt)
                        .messages(history)
                        .preamble(self.system_prompt.clone())
                        .tools(tool_defs.clone()) // ここでツールを登録
                        .additional_params(json!({ "reasoning": { "effort": effort } }))
                        .build();

                    let resp = cm.completion(request).await?;

                    response_blocks.extend(compress(resp.choice.clone()));

                    // モデルの結果を機械に返す
                    let turn = ModelTurn::new(
                        resp.message_id,
                        resp.choice,
                        resp.usage,
                        tool_names.clone(), // executable_tool_names
                        tool_names.clone(), // allowed_tool_names
                    );

                    match run.model_response(turn)? {
                        ModelTurnOutcome::Continue { .. } => {} // 通常はこれ。次の next_step へ
                        ModelTurnOutcome::TurnRetried => {} // リトライ挿入済み。next_step が再度 CallModel を返す
                        ModelTurnOutcome::NeedsResolution(_) => {} // TODO: どうにかする
                    }
                }

                // ツール呼び出し
                AgentRunStep::CallTools { calls } => {
                    let mut results = vec![];

                    for pending in calls {
                        // retryとかで結果が既に確定しているものは、実行せずそのまま返す
                        if let Some(pre) = pending.preresolved_result {
                            results.push(pre);
                            continue;
                        }

                        // ツール実行ここから
                        let call = pending.tool_call;
                        let output: String = self
                            .dispatch_tool(&call.function.name, call.function.arguments)
                            .await;

                        response_blocks.push(Block::ToolCall {
                            name: call.function.name,
                            result: output.clone(),
                        });

                        // 結果は呼び出し ID と 1 対 1 で対応させる。
                        let content = OneOrMany::one(ToolResultContent::text(output));
                        let result = if let Some(call_id) = call.call_id {
                            UserContent::tool_result_with_call_id(call.id, call_id, content)
                        } else {
                            UserContent::tool_result(call.id, content)
                        };
                        results.push(result);
                    }

                    run.tool_results(results)?; // 積んで次の next_step へ（= 再びモデル呼び出し）
                }

                // 最終ステップ
                AgentRunStep::Done(_) => {
                    let out = response_blocks
                        .into_iter()
                        .map(Block::decorate)
                        .collect::<Vec<_>>();
                    return Ok(out.join("\n"));
                }
            }
        }
    }

    /// 内部用。Rig の CompletionClient を使って1回だけのメッセージを生成する。
    async fn run_completion(
        &self,
        model: &str,
        effort: &str,
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
            .additional_params(json!({"reasoning": {"effort": effort}}))
            .build();

        let response = completion_model.completion(request).await?;

        let reply = compress(response.choice)
            .into_iter()
            .map(Block::decorate)
            .collect::<Vec<_>>()
            .join("\n");
        Ok(reply)
    }
}

/// 応答ストリームを、種類ごとの連続ブロックへ圧縮した中間表現。
enum Block {
    Text(String),
    Reasoning(String),
    ToolCall { name: String, result: String }, // ツール呼び出しの表示用（未使用）
}

impl Block {
    /// ブロックを Discord 向けの文字列へ装飾する。
    fn decorate(self) -> String {
        match self {
            Self::Text(text) => text,
            Self::Reasoning(text) => prefix_lines(&text, "> -# "),
            Self::ToolCall { name, result } => {
                prefix_lines(&format!("ツール {} の結果: {}", name, result), "> -# ")
            }
        }
    }
}

/// 連続する同種の content をひとつのブロックにまとめる（圧縮）。
fn compress(choice: impl IntoIterator<Item = AssistantContent>) -> Vec<Block> {
    choice.into_iter().fold(Vec::new(), |mut blocks, content| {
        match content {
            AssistantContent::Text(t) => match blocks.last_mut() {
                Some(Block::Text(buf)) => buf.push_str(&t.text),
                _ => blocks.push(Block::Text(t.text)),
            },
            AssistantContent::Reasoning(r) => {
                let text = r.display_text();
                match blocks.last_mut() {
                    Some(Block::Reasoning(buf)) => {
                        buf.push('\n');
                        buf.push_str(&text);
                    }
                    _ => blocks.push(Block::Reasoning(text)),
                }
            }
            AssistantContent::ToolCall(_) => {} // ツール呼び出しは AgentRunStep::CallTools で処理する
            AssistantContent::Image(_) => {}
        }
        blocks
    })
}

/// 各行の先頭に prefix を付ける。
fn prefix_lines(text: &str, prefix: &str) -> String {
    text.lines()
        .map(|line| format!("{prefix}{line}"))
        .collect::<Vec<_>>()
        .join("\n")
}

mod tools {
    pub(super) fn watch() -> String {
        let now = chrono::Utc::now();
        let formatted_time = now.format("%Y-%m-%d %H:%M:%S").to_string();
        format!("まなみの時計によると、現在時刻は {formatted_time} だよ！")
    }

    pub(super) fn watch_definition() -> super::ToolDefinition {
        super::ToolDefinition {
            name: "watch".into(),
            description: "現在時刻を確認したいときに使用できます。".into(),
            parameters: serde_json::json!({
                "type": "object",
                "properties": {}
            }),
        }
    }

    pub(super) async fn web_search_wikipedia(query: &str, lang: &str) -> Result<String, String> {
        let url = format!(
            "https://{lang}.wikipedia.org/w/api.php?action=query&list=search&srsearch={query}&format=json"
        );
        let resp = reqwest::get(&url)
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
                let snippet = result["snippet"].as_str().unwrap_or("");
                format!("- {title}: {snippet}")
            })
            .collect::<Vec<_>>()
            .join("\n");

        Ok(format!(
            "Wikipedia での「{query}」の検索結果だよ:\n{}",
            search_results
        ))
    }

    pub(super) fn web_search_wikipedia_definition() -> super::ToolDefinition {
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

    pub(super) async fn web_fetch_wikipedia(title: &str, lang: &str) -> Result<String, String> {
        let url = format!(
            "https://{lang}.wikipedia.org/w/api.php?action=query&prop=extracts&titles={title}&format=json"
        );
        let resp = reqwest::get(&url)
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

        Ok(format!("Wikipedia の「{title}」の内容だよ:\n{}", extract))
    }

    pub(super) fn web_fetch_wikipedia_definition() -> super::ToolDefinition {
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
        // ユーザー発言は話者名を Role に保持し、content は本文そのもの。
        // 話者名の付与は LLM 送信時（to_rig）に行う。
        let user_msg = ChatMessage::user("宇田", "やあ");
        match &user_msg.role {
            Role::User { name } => assert_eq!(name.as_str(), "宇田"),
            Role::Assistant => panic!("expected a user role"),
        }
        assert_eq!(user_msg.content, "やあ");

        // まなみ（アシスタント）の発言は名前を持たず、content は本文そのもの。
        let assistant_msg = ChatMessage::assistant("やっほー");
        assert!(matches!(assistant_msg.role, Role::Assistant));
        assert_eq!(assistant_msg.content, "やっほー");
    }
}
