//! まなみのLLM機能を提供するモジュール。
//! LLM AgentのLoop、ツールの呼び出し、会話履歴の管理などを行う。

use std::collections::{HashMap, VecDeque};
use std::sync::Mutex;

use anyhow::Result;
use chrono::{DateTime, Utc};

use rig::providers::openai;

mod decorate_output;
mod engine;
mod message;
mod models;
mod prompt;
mod tools;

pub use message::{stamp_jst, ChatMessage, Role};
pub use models::available_models;

use prompt::{MANAMI_PROMPT, MATOME_PROMPT, MEMORY_SUMMARY_PROMPT};

pub struct ManamiAi {
    client: openai::Client,
    default_model: String,
    model: Mutex<String>,
    effort: Mutex<String>,
    system_prompt: String,
    // 会話バッファはチャンネルごとに分ける。キーは Discord のチャンネル ID（ChannelId::get() の u64）。
    conversations: Mutex<HashMap<u64, VecDeque<ChatMessage>>>,
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
            conversations: Mutex::new(HashMap::new()),
        })
    }

    /// 指定チャンネルの会話バッファにユーザー発言を追加する。話者名・時刻はLLM 送信時（to_rig）に本文の先頭へ付与する。
    /// `timestamp` は発言時刻（Discord 送信時刻）を UTC で渡す。
    pub fn add_user_log(
        &self,
        channel_id: u64,
        user: &str,
        message: &str,
        timestamp: DateTime<Utc>,
    ) {
        self.push(channel_id, ChatMessage::user(user, message, timestamp));
    }

    /// 指定チャンネルの会話バッファにまなみの発言を追加する。時刻は返信生成時刻 (UTC).
    pub fn add_model_log(&self, channel_id: u64, message: &str) {
        self.push(channel_id, ChatMessage::assistant(message, Utc::now()));
    }

    /// 指定チャンネルの会話バッファにメッセージを追加する。チャンネルごとに最大 500 件まで保持する。
    // buf は map（ロックガード）から借用するので、ガードは本文全体で保持する必要がある。
    #[allow(clippy::significant_drop_tightening)]
    fn push(&self, channel_id: u64, message: ChatMessage) {
        let mut map = self.conversations.lock().unwrap();
        let buf = map.entry(channel_id).or_default();
        buf.push_back(message);
        if buf.len() > 500 {
            buf.pop_front();
        }
    }

    /// 指定チャンネルの会話バッファを消す。他チャンネルのログには触れない。
    pub fn clear(&self, channel_id: u64) {
        self.conversations.lock().unwrap().remove(&channel_id);
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
    /// `target_user_id` は「いま応答している相手」の Discord user_id。
    pub async fn generate(
        &self,
        db: &crate::db::BotDatabase,
        target_user_id: &str,
        channel_id: u64,
    ) -> Result<String> {
        let model = self.get_model();
        self.generate_with_model(&model, db, target_user_id, channel_id)
            .await
    }

    /// 現在のモデルを使ってメッセージを生成する。モデル名が空文字列なら現在のモデルにフォールバックする。
    /// `target_user_id` の人間プロフィールを取得し、システムプロンプト末尾へ決定的に注入する。
    pub async fn generate_with_model(
        &self,
        model: &str,
        db: &crate::db::BotDatabase,
        target_user_id: &str,
        channel_id: u64,
    ) -> Result<String> {
        // 全レスモード未設定時などモデルが空なら、現在のモデルにフォールバックする。
        let model = if model.trim().is_empty() {
            self.get_model()
        } else {
            model.to_owned()
        };

        let effort = self.effort.lock().unwrap().clone();

        // ロックは await をまたがず、ここでコピーして解放する。
        // 混線防止のため、対象チャンネルのバッファだけを読む。
        let messages: Vec<ChatMessage> = self
            .conversations
            .lock()
            .unwrap()
            .get(&channel_id)
            .map(|buf| buf.iter().cloned().collect())
            .unwrap_or_default();

        if messages.is_empty() {
            return Ok("やっほー！　どうしたの？".to_owned());
        }

        // 応答相手のプロフィールを決定的に引き、システムプロンプト末尾へ注入する。
        // 取得失敗や未登録は握り潰し、素のペルソナのまま会話を止めない。
        // target_user_id が空のときは「応答相手が会話文脈に居ない」ケース（呼び出し側参照）なので、
        // 取り違え注入を避けるためプロフィールは引かない。
        let profile_block = if target_user_id.is_empty() {
            String::new()
        } else {
            db.fetch_user_profile(target_user_id)
                .await
                .ok()
                .flatten()
                .map(|profile| profile.to_prompt())
                .unwrap_or_default()
        };
        let system_prompt = format!("{}\n\n{}", self.system_prompt, profile_block.trim());

        let reply = engine::run_agent(
            &self.client,
            &model,
            &effort,
            &system_prompt,
            messages,
            db,
            target_user_id,
        )
        .await?;
        // まなみが履歴を模倣して先頭に付けてしまう時刻・名前の接頭辞を剥がす。
        // ここで正規化してからバッファへ積むことで、次ターンの二重焼き（[時刻][時刻]…）も防ぐ。
        let reply = decorate_output::strip_leading_prefix(&reply);
        self.add_model_log(channel_id, &reply);
        Ok(reply)
    }

    /// チャットログを渡して要約させる。
    pub async fn generate_matome(&self, messages: Vec<ChatMessage>) -> Result<String> {
        if messages.is_empty() {
            return Ok("まとめるログがないよ".to_owned());
        }
        let model = self.default_model.clone();
        let effort = "low";
        engine::run_completion(&self.client, &model, effort, MATOME_PROMPT, messages).await
    }

    /// 会話セッションを長期記憶用に要約する。記憶に値しないとモデルが判断したときは `None`。
    ///
    /// `channel_name` はプロンプト末尾に添える。ログ本文にはチャンネル名が含まれないので、
    /// これを渡さないとモデルは「どのチャンネルの会話か」を判断できない
    /// (夢日記チャンネルの話を事実として要約してしまう)。
    pub async fn generate_memory_summary(
        &self,
        channel_name: &str,
        messages: Vec<ChatMessage>,
    ) -> Result<Option<String>> {
        if messages.is_empty() {
            return Ok(None);
        }
        let model = self.default_model.clone();
        let preamble = format!("{MEMORY_SUMMARY_PROMPT}\n\n## このログのチャンネル\n#{channel_name}");
        let summary =
            engine::run_completion_text(&self.client, &model, "low", &preamble, messages).await?;

        let summary = summary.trim();
        if summary.is_empty() || summary == "SKIP" {
            Ok(None)
        } else {
            Ok(Some(summary.to_owned()))
        }
    }
}
