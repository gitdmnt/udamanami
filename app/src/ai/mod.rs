//! まなみのLLM機能を提供するモジュール。
//! LLM AgentのLoop、ツールの呼び出し、会話履歴の管理などを行う。

use std::collections::{HashMap, VecDeque};
use std::sync::Mutex;

use anyhow::Result;
use chrono::{DateTime, TimeDelta, Utc};
use tracing::{info, warn};

use rig::providers::openai;

mod decorate_output;
mod engine;
mod message;
mod models;
mod plan;
mod prompt;
mod tools;

pub use message::{stamp_jst, ChatMessage, Role};
pub use models::available_models;

use decorate_output::Block;
use plan::{fallback_plan, ResponsePlan};
use prompt::{MANAMI_PROMPT, MATOME_PROMPT, MEMORY_SUMMARY_PROMPT, PLANNER_PROMPT};

/// 会話バッファがチャンネルごとに保持するチャットログの数。
const BUFFER_LEN: usize = 10;

/// 同じ話者の連投を1つのログに圧縮する間隔の上限。
const MERGE_GAP: TimeDelta = TimeDelta::minutes(3);

/// 第2段へ渡すチャットログの件数。
const PERFORMER_WINDOW: usize = 4;

/// 応答生成のstage。stageごとにモデルと reasoning effort を切り替える。
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Stage {
    /// 内容決定
    Planner,
    /// RP
    Performer,
}

impl Stage {
    /// スラッシュコマンドの選択肢から段を引く。未知の値は `None`。
    pub fn parse(name: &str) -> Option<Self> {
        match name {
            "planner" => Some(Self::Planner),
            "performer" => Some(Self::Performer),
            _ => None,
        }
    }

    /// スラッシュコマンドの選択肢と、応答メッセージに出す表記。
    pub const fn label(self) -> &'static str {
        match self {
            Self::Planner => "planner",
            Self::Performer => "performer",
        }
    }
}

/// 各stageのモデルと reasoning effort。
struct StageConfig {
    model: String,
    effort: String,
}

pub struct ManamiAi {
    client: openai::Client,
    /// 要約系(matome / memory summary)が使うモデル。stageの切り替えとは独立に固定する。
    default_model: String,
    planner: Mutex<StageConfig>,
    performer: Mutex<StageConfig>,
    // 会話バッファはチャンネルごとに分ける。キーは Discord のチャンネル ID（ChannelId::get() の u64）。
    conversations: Mutex<HashMap<u64, VecDeque<ChatMessage>>>,
}

impl ManamiAi {
    /// まなみのペルソナ付きでチャットボットのクライアントを構築する。
    pub fn manami(
        base_url: &str,
        api_key: &str,
        planner_model: &str,
        planner_effort: &str,
        performer_model: &str,
        performer_effort: &str,
    ) -> Result<Self> {
        // OpenAI 互換の Responses API クライアント（POST {base_url}/responses）。
        let client = openai::Client::builder()
            .api_key(api_key)
            .base_url(base_url)
            .build()
            .map_err(|e| anyhow::anyhow!("failed to build LLM client: {e:?}"))?;

        Ok(Self {
            client,
            default_model: planner_model.to_owned(),
            planner: Mutex::new(StageConfig {
                model: planner_model.to_owned(),
                effort: planner_effort.to_owned(),
            }),
            performer: Mutex::new(StageConfig {
                model: performer_model.to_owned(),
                effort: performer_effort.to_owned(),
            }),
            conversations: Mutex::new(HashMap::new()),
        })
    }

    /// 指定チャンネルの会話バッファにユーザー発言を追加する。話者名・時刻はLLM 送信時（to_rig）に本文の先頭へ付与する。
    /// `user_id` は連投の判定に使う Discord の user_id。
    /// `timestamp` は発言時刻（Discord 送信時刻）を UTC で渡す。
    pub fn add_user_log(
        &self,
        channel_id: u64,
        user_id: u64,
        user: &str,
        message: &str,
        timestamp: DateTime<Utc>,
    ) {
        self.push(
            channel_id,
            ChatMessage::user(user_id, user, message, timestamp),
        );
    }

    /// 指定チャンネルの会話バッファにまなみの発言を追加する。時刻は返信生成時刻 (UTC).
    pub fn add_model_log(&self, channel_id: u64, message: &str) {
        self.push(channel_id, ChatMessage::assistant(message, Utc::now()));
    }

    /// 指定チャンネルの会話バッファにメッセージを追加する。
    ///
    /// 同じ話者の連投は新しい要素を積まず、直前の本文へ繋げる（[`ChatMessage::can_merge`]）。
    // buf は map（ロックガード）から借用するので、ガードは本文全体で保持する必要がある。
    #[allow(clippy::significant_drop_tightening)]
    fn push(&self, channel_id: u64, message: ChatMessage) {
        let mut map = self.conversations.lock().unwrap();
        let buf = map.entry(channel_id).or_default();

        if let Some(last) = buf.back_mut() {
            if last.can_merge(&message, MERGE_GAP) {
                last.merge(&message);
                return;
            }
        }

        buf.push_back(message);
        if buf.len() > BUFFER_LEN {
            buf.pop_front();
        }
    }

    /// 指定チャンネルの会話バッファを消す。他チャンネルのログには触れない。
    pub fn clear(&self, channel_id: u64) {
        self.conversations.lock().unwrap().remove(&channel_id);
    }

    /// 指定したstageのモデルを変更する。会話バッファはクリアされない。
    pub fn set_model(&self, stage: Stage, model: String) {
        self.stage(stage).lock().unwrap().model = model;
    }

    /// 指定したstageのモデルを取得する。
    pub fn get_model(&self, stage: Stage) -> String {
        self.stage(stage).lock().unwrap().model.clone()
    }

    /// 指定したstageのreasoning effortを変更する。会話バッファはクリアされない。
    pub fn set_effort(&self, stage: Stage, effort: String) {
        self.stage(stage).lock().unwrap().effort = effort;
    }

    /// 指定したstageのreasoning effortを取得する。
    pub fn get_effort(&self, stage: Stage) -> String {
        self.stage(stage).lock().unwrap().effort.clone()
    }

    const fn stage(&self, stage: Stage) -> &Mutex<StageConfig> {
        match stage {
            Stage::Planner => &self.planner,
            Stage::Performer => &self.performer,
        }
    }

    /// モデルと effort をコピーして取り出す。ロックは await をまたがせない。
    fn stage_config(&self, stage: Stage) -> (String, String) {
        let config = self.stage(stage).lock().unwrap();
        (config.model.clone(), config.effort.clone())
    }

    /// 会話バッファを読んで応答を生成する。
    /// `target_user_id` は「いま応答している相手」の Discord user_id。
    /// `addressed` は名指しで呼ばれたかどうかで、trueのときは必ず応答を生成する。
    pub async fn generate<G>(
        &self,
        db: &crate::db::BotDatabase,
        target_user_id: &str,
        channel_id: u64,
        addressed: bool,
        on_reply_decided: impl FnOnce() -> G,
    ) -> Result<Option<String>> {
        // 混線防止のため、対象チャンネルのバッファだけを読む。
        let messages = self.snapshot(channel_id);
        if messages.is_empty() {
            let _typing = on_reply_decided();
            return Ok(Some("やっほー！　どうしたの？".to_owned()));
        }

        let profile_block = self.profile_block(db, target_user_id).await;

        let (mut plan, trace) = self
            .run_planner(
                db,
                target_user_id,
                channel_id,
                addressed,
                &messages,
                &profile_block,
            )
            .await;

        if !plan.should_reply && !addressed {
            return Ok(None);
        }

        if plan.points.is_empty() {
            plan.points = fallback_plan(last_speaker(&messages)).points;
        }

        // ガードは第2段が終わるまで生かす。ここで捨てると打鍵表示が即座に消える。
        let _typing = on_reply_decided();

        let reply = self.run_performer(&plan, &messages, &profile_block).await?;
        // まなみが履歴を模倣して先頭に付けてしまう時刻・名前の接頭辞を剥がす。
        // ここで正規化してからバッファへ積むことで、次ターンの二重焼き（[時刻][時刻]…）も防ぐ。
        let reply = decorate_output::strip_leading_prefix(&reply);
        self.add_model_log(channel_id, &reply);

        Ok(Some(compose(trace, reply)))
    }

    /// 内容を生成するstage.
    async fn run_planner(
        &self,
        db: &crate::db::BotDatabase,
        target_user_id: &str,
        channel_id: u64,
        addressed: bool,
        messages: &[ChatMessage],
        profile_block: &str,
    ) -> (ResponsePlan, Vec<Block>) {
        let (model, effort) = self.stage_config(Stage::Planner);
        let preamble = format!(
            "{PLANNER_PROMPT}\n\n## いまの状況\n- {}\n\n{}",
            if addressed {
                "まなみは名指しで呼ばれている。必ず返信すること(should_reply は true)。"
            } else {
                "まなみは名指しで呼ばれていない。会話に入るべきかどうかから判断すること。"
            },
            profile_block.trim()
        );

        let run = engine::run_agent_typed::<ResponsePlan>(
            &self.client,
            &model,
            &effort,
            &preamble,
            messages.to_vec(),
            db,
            target_user_id,
        )
        .await;

        match run {
            Ok(run) => {
                info!(
                    target: "manami::plan",
                    channel_id,
                    user_id = target_user_id,
                    model = %model,
                    should_reply = run.value.should_reply,
                    plan = %run.raw,
                    "planner",
                );
                (run.value, run.blocks)
            }
            Err(e) => {
                warn!(
                    target: "manami::plan",
                    channel_id,
                    user_id = target_user_id,
                    model = %model,
                    error = %e,
                    "planner failed; falling back",
                );
                (fallback_plan(last_speaker(messages)), Vec::new())
            }
        }
    }

    /// 第2段。RPを生成する。
    async fn run_performer(
        &self,
        plan: &ResponsePlan,
        messages: &[ChatMessage],
        profile_block: &str,
    ) -> Result<String> {
        let (model, effort) = self.stage_config(Stage::Performer);
        let preamble = format!("{MANAMI_PROMPT}\n\n{}", profile_block.trim());
        let window = messages[messages.len().saturating_sub(PERFORMER_WINDOW)..].to_vec();

        engine::run_performer(
            &self.client,
            &model,
            &effort,
            &preamble,
            window,
            &plan.render(),
        )
        .await
    }

    /// 指定チャンネルのバッファを複製する。ロックは await をまたがせない。
    fn snapshot(&self, channel_id: u64) -> Vec<ChatMessage> {
        self.conversations
            .lock()
            .unwrap()
            .get(&channel_id)
            .map(|buf| buf.iter().cloned().collect())
            .unwrap_or_default()
    }

    /// 応答相手のプロフィールを参照する。
    async fn profile_block(&self, db: &crate::db::BotDatabase, target_user_id: &str) -> String {
        if target_user_id.is_empty() {
            return String::new();
        }
        db.fetch_user_profile(target_user_id)
            .await
            .ok()
            .flatten()
            .map(|profile| profile.to_prompt())
            .unwrap_or_default()
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
        let preamble =
            format!("{MEMORY_SUMMARY_PROMPT}\n\n## このログのチャンネル\n#{channel_name}");
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

/// 第1段の痕跡を Discord 向けに整え、本文の前に置く。
/// Text ブロックはJSONなので捨てちゃう。
fn compose(trace: Vec<Block>, reply: String) -> String {
    let mut out: Vec<String> = trace
        .into_iter()
        .filter(|block| !matches!(block, Block::Text(_)))
        .map(Block::decorate)
        .collect();
    out.push(reply);
    out.join("\n")
}

/// ユーザー発言の話者名。まなみの発言しか無ければ空文字列。
fn last_speaker(messages: &[ChatMessage]) -> &str {
    messages
        .iter()
        .rev()
        .find_map(|m| match &m.role {
            Role::User { name, .. } => Some(name.as_str()),
            Role::Assistant => None,
        })
        .unwrap_or("")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ai() -> ManamiAi {
        ManamiAi::manami(
            "https://example.invalid/v1",
            "dummy",
            "planner-model",
            "medium",
            "performer-model",
            "none",
        )
        .unwrap()
    }

    /// 2026-07-17T03:34:00Z から `minutes` 分進めた時刻。
    fn at(minutes: i64) -> DateTime<Utc> {
        DateTime::parse_from_rfc3339("2026-07-17T03:34:00Z")
            .unwrap()
            .with_timezone(&Utc)
            + TimeDelta::minutes(minutes)
    }

    const UDA: u64 = 1;
    const USAMIMU: u64 = 2;

    /// バッファの (話者ラベル, 本文) を並び順で取り出す。
    fn buffer(ai: &ManamiAi, channel_id: u64) -> Vec<(String, String)> {
        ai.snapshot(channel_id)
            .into_iter()
            .map(|m| {
                let who = match &m.role {
                    Role::User { name, .. } => name.clone(),
                    Role::Assistant => "まなみ".to_owned(),
                };
                (who, m.content)
            })
            .collect()
    }

    #[test]
    fn consecutive_messages_from_one_speaker_merge() {
        let ai = ai();
        ai.add_user_log(1, UDA, "宇田", "スピーカー買った", at(0));
        ai.add_user_log(1, UDA, "宇田", "Bluetoothだった", at(1));
        ai.add_user_log(1, USAMIMU, "うさみむ", "いいね", at(2));

        assert_eq!(
            buffer(&ai, 1),
            vec![
                (
                    "宇田".to_owned(),
                    "スピーカー買った\nBluetoothだった".to_owned()
                ),
                ("うさみむ".to_owned(), "いいね".to_owned()),
            ]
        );
    }

    #[test]
    fn a_long_pause_starts_a_new_message() {
        let ai = ai();
        ai.add_user_log(1, UDA, "宇田", "ほげ", at(0));
        ai.add_user_log(1, UDA, "宇田", "ふが", at(60));

        // 1時間開いた一言を1つの発言にすると、まなみが会話の間合いを読み違える。
        assert_eq!(buffer(&ai, 1).len(), 2);
    }

    #[test]
    fn merging_does_not_evict_older_messages() {
        let ai = ai();
        // 話者を交互にして BUFFER_LEN ちょうどまで埋める。
        for i in 0..BUFFER_LEN {
            let (id, who) = if i % 2 == 0 {
                (UDA, "宇田")
            } else {
                (USAMIMU, "うさみむ")
            };
            ai.add_user_log(1, id, who, &format!("{i}"), at(i as i64));
        }
        assert_eq!(buffer(&ai, 1).len(), BUFFER_LEN);

        // 連投は要素を増やさないので、先頭が押し出されてはいけない。
        ai.add_user_log(1, USAMIMU, "うさみむ", "つづき", at(BUFFER_LEN as i64));
        let after = buffer(&ai, 1);
        assert_eq!(after.len(), BUFFER_LEN);
        assert_eq!(after[0].1, "0");
        assert_eq!(after[BUFFER_LEN - 1].1, "9\nつづき");

        // 別の話者なら積まれ、先頭が落ちる。
        ai.add_user_log(1, UDA, "宇田", "わりこみ", at(BUFFER_LEN as i64 + 1));
        let after = buffer(&ai, 1);
        assert_eq!(after.len(), BUFFER_LEN);
        assert_eq!(after[0].1, "1");
    }

    #[test]
    fn buffers_do_not_leak_across_channels() {
        let ai = ai();
        ai.add_user_log(1, UDA, "宇田", "こっち", at(0));
        ai.add_user_log(2, UDA, "宇田", "あっち", at(0));

        assert_eq!(buffer(&ai, 1).len(), 1);
        assert_eq!(buffer(&ai, 2).len(), 1);
        ai.clear(1);
        assert!(buffer(&ai, 1).is_empty());
        assert_eq!(buffer(&ai, 2).len(), 1);
    }

    #[test]
    fn stage_settings_are_independent() {
        let ai = ai();
        assert_eq!(ai.get_model(Stage::Planner), "planner-model");
        assert_eq!(ai.get_effort(Stage::Performer), "none");

        ai.set_model(Stage::Performer, "another".to_owned());
        assert_eq!(ai.get_model(Stage::Performer), "another");
        // 片方を変えても、もう片方は動かない。
        assert_eq!(ai.get_model(Stage::Planner), "planner-model");
    }

    #[test]
    fn stage_parses_only_known_names() {
        assert_eq!(Stage::parse("planner"), Some(Stage::Planner));
        assert_eq!(Stage::parse("performer"), Some(Stage::Performer));
        assert_eq!(Stage::parse("Planner"), None);
        assert_eq!(Stage::parse(""), None);
    }

    #[test]
    fn compose_drops_the_structured_output_and_keeps_the_trace() {
        let trace = vec![
            Block::Reasoning("考えた".to_owned()),
            Block::Text("{\"should_reply\":true}".to_owned()),
            Block::ToolCall {
                name: "recall".to_owned(),
                args: "{}".to_owned(),
            },
        ];
        let out = compose(trace, "やっほー！".to_owned());

        // 構造化データが Discord へ漏れない。
        assert!(!out.contains("should_reply"));
        assert!(out.contains("> -# 考えた"));
        assert!(out.contains("ツール recall を使ったよ"));
        assert!(out.ends_with("やっほー！"));
    }

    #[test]
    fn last_speaker_skips_manami() {
        let messages = vec![
            ChatMessage::user(UDA, "宇田", "やあ", at(0)),
            ChatMessage::assistant("やっほー", at(1)),
        ];
        assert_eq!(last_speaker(&messages), "宇田");
        assert_eq!(last_speaker(&messages[1..]), "");
        assert_eq!(last_speaker(&[]), "");
    }
}
