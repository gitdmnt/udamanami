//! まなみのLLM機能を提供するモジュール。
//! LLM AgentのLoop、ツールの呼び出し、会話履歴の管理などを行う。

use std::collections::VecDeque;
use std::sync::Mutex;

use anyhow::Result;

use rig::providers::openai;

mod decorate_output;
mod engine;
mod message;
mod models;
mod prompt;
mod tools;

pub use message::{ChatMessage, Role};
pub use models::available_models;

use prompt::{MANAMI_PROMPT, MATOME_PROMPT};

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
    pub async fn generate(&self, db: &crate::db::BotDatabase) -> Result<String> {
        let model = self.get_model();
        self.generate_with_model(&model, db).await
    }

    /// 現在のモデルを使ってメッセージを生成する。モデル名が空文字列なら現在のモデルにフォールバックする。
    pub async fn generate_with_model(
        &self,
        model: &str,
        db: &crate::db::BotDatabase,
    ) -> Result<String> {
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

        let reply = engine::run_agent(
            &self.client,
            &model,
            &effort,
            &self.system_prompt,
            messages,
            db,
        )
        .await?;
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
        engine::run_completion(&self.client, &model, effort, MATOME_PROMPT, messages).await
    }
}
