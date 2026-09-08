//! チャンネルの会話セッションを自動要約して memory 化する常駐タスク。
//!
//! IDLE_GAP 分発言がないか、未要約のログが SESSION_MESSAGE_LIMIT 件たまったときに発火する。

use std::sync::Arc;

use chrono::{DateTime, TimeDelta, Utc};
use dashmap::DashMap;
use serenity::model::id::{ChannelId, UserId};
use tokio::time::{interval, Duration, MissedTickBehavior};
use tracing::{error, info, warn};

use crate::ai::ChatMessage;
use crate::db::MessageInfo;
use crate::Bot;

/// 件数チェックの間隔。処理は D1 への1クエリだけなので短くてよい。
const TICK: Duration = Duration::from_secs(5 * 60);
const IDLE_GAP: TimeDelta = TimeDelta::hours(6);
const SESSION_MESSAGE_LIMIT: u32 = 100;
const MAX_MESSAGES_PER_RUN: usize = 200;

/// 1 回の要約で何回 LLM API を叩けちゃうのか
const MAX_CHANNELS_PER_TICK: usize = 3;
/// 連続失敗がこの回数に達したチャンネルは自動要約から外す (API 叩きに失敗されると困るため)。
const MAX_CONSECUTIVE_FAILURES: u32 = 5;
/// 要約する価値があるとみなす人間の発言数の下限。
const MIN_HUMAN_MESSAGES: usize = 1;
/// 要約する価値があるとみなす人間の発言の総文字数の下限。
const MIN_HUMAN_CHARS: usize = 100;

/// 1チャンネル分の要約結果。
#[derive(Debug, PartialEq, Eq)]
pub enum Outcome {
    /// 記憶を作った。
    Summarized { title: String },
    /// 要約する価値が無かった。進捗は前進させる。
    Skipped(&'static str),
    /// 未要約の範囲が空だった。進捗は動かさない。
    Nothing,
}

/// 常駐タスク本体。TICK ごとに候補を引き、区切りが来たチャンネルを要約する。
pub async fn run(bot: Arc<Bot>, http: Arc<serenity::http::Http>) {
    let my_userid = loop {
        match http.get_current_user().await {
            Ok(user) => break user.id,
            Err(e) => {
                warn!("summarizer: failed to get current user, retrying: {e:?}");
                tokio::time::sleep(Duration::from_secs(30)).await;
            }
        }
    };
    info!("summarizer: started (self={my_userid})");

    // 各チャンネルが要約に連続で失敗した回数。
    let failures: DashMap<ChannelId, u32> = DashMap::new();

    let mut ticker = interval(TICK);
    ticker.set_missed_tick_behavior(MissedTickBehavior::Delay);
    loop {
        ticker.tick().await;
        tick_once(&bot, &http, my_userid, &failures).await;
    }
}

/// 1 tick 分の処理。候補を引いて先頭から順に要約する。
async fn tick_once(
    bot: &Bot,
    http: &serenity::http::Http,
    my_userid: UserId,
    failures: &DashMap<ChannelId, u32>,
) {
    let now = Utc::now();
    let candidates = match bot
        .database
        .fetch_summarize_candidates(now - IDLE_GAP, SESSION_MESSAGE_LIMIT)
        .await
    {
        Ok(candidates) => candidates,
        Err(e) => {
            error!("summarizer: failed to fetch candidates: {e:?}");
            return;
        }
    };

    let found = candidates.len();
    let mut attempted = 0_usize;

    for candidate in candidates {
        if attempted >= MAX_CHANNELS_PER_TICK {
            break;
        }
        let Ok(channel_id) = candidate.channel_id.parse::<u64>().map(ChannelId::from) else {
            error!("summarizer: invalid channel_id: {}", candidate.channel_id);
            continue;
        };

        if failures
            .get(&channel_id)
            .is_some_and(|strikes| *strikes >= MAX_CONSECUTIVE_FAILURES)
        {
            continue;
        }

        attempted += 1;
        match summarize_channel(bot, my_userid, &candidate, now).await {
            Ok(outcome) => {
                failures.remove(&channel_id);
                match outcome {
                    Outcome::Summarized { title } => {
                        info!("summarizer: remembered {title}");
                    }
                    Outcome::Skipped(reason) => {
                        info!("summarizer: skipped #{} ({reason})", candidate.name);
                    }
                    Outcome::Nothing => {}
                }
            }
            Err(e) => {
                // 進捗は前進していないので、次の機会に同じ範囲がリトライされる。
                let strikes = {
                    let mut entry = failures.entry(channel_id).or_insert(0);
                    *entry += 1;
                    *entry
                };
                error!(
                    "summarizer: failed on #{} ({strikes}/{MAX_CONSECUTIVE_FAILURES}): {e:?}",
                    candidate.name
                );。
                if strikes == MAX_CONSECUTIVE_FAILURES {
                    error!(
                        "summarizer: giving up on #{} after {strikes} consecutive failures; \
                         restart the bot after fixing the cause",
                        candidate.name
                    );
                    notify_given_up(bot, http, &candidate, strikes, &e).await;
                }
            }
        }
    }

    // このtick行が出ていなければ、そもそも要約タスクが回っていない。
    info!("summarizer: tick candidates={found} attempted={attempted}");
}

/// 打ち切りをデバッグチャンネルで通知するよ
async fn notify_given_up(
    bot: &Bot,
    http: &serenity::http::Http,
    candidate: &udamanami_shared::SummarizeCandidate,
    strikes: u32,
    error: &anyhow::Error,
) {
    if bot.debug_channel_id == ChannelId::default() {
        return;
    }

    // Discord の2000文字上限に当たると通知ごと落ちるからエラーの本文は省略する
    let detail: String = format!("{error:?}").chars().take(500).collect();
    let message = format!(
        "自動要約を停止しました。\n\
         チャンネル: #{} ({})\n\
         連続失敗: {strikes}回\n\
         最後のエラー: {detail}\n\
         原因を直して bot を再起動するまで、このチャンネルの自動要約は止まったままです。",
        channel_name(candidate),
        candidate.channel_id,
    );
    if let Err(e) = bot.debug_channel_id.say(http, message).await {
        warn!("summarizer: failed to notify debug channel: {e:?}");
    }
}

/// 1チャンネル分の未要約範囲を要約して記憶に落とす。
/// 進捗(`last_summarized_at`)を前進させるのは、記憶を登録できたときと、要約する価値が無いと判断したときだけ
pub async fn summarize_channel(
    bot: &Bot,
    my_userid: UserId,
    candidate: &udamanami_shared::SummarizeCandidate,
    _now: DateTime<Utc>,
) -> anyhow::Result<Outcome> {
    let channel_id = ChannelId::from(candidate.channel_id.parse::<u64>()?);
    let from = candidate
        .last_summarized_at
        .as_deref()
        .and_then(|s| DateTime::parse_from_rfc3339(s).ok())
        .map(|t| t.with_timezone(&Utc));

    let messages = bot
        .database
        .fetch_log_by_range(
            &channel_id,
            from,
            candidate.last_summarized_message_id.clone(),
            None,
            MAX_MESSAGES_PER_RUN,
        )
        .await?;

    let Some(last) = messages.last() else {
        return Ok(Outcome::Nothing);
    };
    // 進めるのは「実際に取れた最後の発言」まで。候補の最新未要約時刻まで進めると、
    // MAX_MESSAGES_PER_RUN で切られた分を要約せずに飛ばしてしまう。
    let session_end = last.timestamp;
    let session_end_message_id = last.message_id;
    let session_start = messages[0].timestamp;
    let message_ids = messages.iter().map(|message| message.message_id).collect();

    if let Some(reason) = skip_reason(&messages, my_userid) {
        bot.database
            .set_channel_summarized(
                &channel_id,
                session_end,
                session_end_message_id,
                message_ids,
            )
            .await?;
        return Ok(Outcome::Skipped(reason));
    }

    let channel_name = channel_name(candidate);
    let chat: Vec<ChatMessage> = messages
        .iter()
        .map(|m| m.to_chat_message(&my_userid))
        .collect();

    let Some(summary) = bot.ai.generate_memory_summary(&channel_name, chat).await? else {
        bot.database
            .set_channel_summarized(
                &channel_id,
                session_end,
                session_end_message_id,
                message_ids,
            )
            .await?;
        return Ok(Outcome::Skipped("モデルが記憶に値しないと判断した"));
    };

    // 出所(チャンネル名・会話日時)は本文と別の列に持たせる。夢日記の話を事実として語らせない文脈。
    let title = build_title(&channel_name);
    bot.database
        .create_summary_memory(&title, summary.trim(), &channel_name, session_start)
        .await?;
    bot.database
        .set_channel_summarized(
            &channel_id,
            session_end,
            session_end_message_id,
            message_ids,
        )
        .await?;

    Ok(Outcome::Summarized { title })
}

/// 通常 tick が待つ無音/件数の区切りを飛ばし、指定チャンネルを即座に候補化する(!summarize 用)。
///
/// idle_before を未来にして workers 側の無音条件を必ず真にし、min_pending=1 で件数下限を落とす。
/// 未要約が 0 件なら None(= 無変動なら要約しない不変条件は保つ)。
pub async fn on_demand_candidate(
    bot: &Bot,
    channel_id: ChannelId,
    now: DateTime<Utc>,
) -> anyhow::Result<Option<udamanami_shared::SummarizeCandidate>> {
    let candidates = bot
        .database
        .fetch_summarize_candidates(now + TimeDelta::days(1), 1)
        .await?;
    let channel_id = channel_id.get().to_string();
    Ok(candidates.into_iter().find(|c| c.channel_id == channel_id))
}

/// まなみでも bot コマンドでもない、人間の地の発言だけを見る。
fn is_human_talk(message: &MessageInfo, my_userid: UserId) -> bool {
    message.user_id != my_userid && !message.content.starts_with('!')
}

/// 要約をスキップすべきなら理由を返す。スキップは失敗ではないので、進捗は前進させる。
/// 進めないと、数発言だけのチャンネルが候補に残り続け、後日「何日ぶんもの範囲」として要約される。
fn skip_reason(messages: &[MessageInfo], my_userid: UserId) -> Option<&'static str> {
    let human: Vec<&MessageInfo> = messages
        .iter()
        .filter(|m| is_human_talk(m, my_userid))
        .collect();

    if human.len() < MIN_HUMAN_MESSAGES {
        return Some("人間の発言が少なすぎる");
    }
    if human
        .iter()
        .map(|m| m.content.chars().count())
        .sum::<usize>()
        < MIN_HUMAN_CHARS
    {
        return Some("人間の発言が短すぎる");
    }
    None
}

/// チャンネル表示名。`PUT /channel/summary` のプレースホルダ INSERT が name='' の行を作りうるので、空なら ID にフォールバックする。
fn channel_name(candidate: &udamanami_shared::SummarizeCandidate) -> String {
    if candidate.name.trim().is_empty() {
        candidate.channel_id.clone()
    } else {
        candidate.name.clone()
    }
}

/// 記憶のタイトル。日時は occurred_at 列で持つので、ここはチャンネルだけ。
fn build_title(channel_name: &str) -> String {
    format!("#{channel_name} の会話")
}

#[cfg(test)]
mod tests {
    use super::*;
    use udamanami_shared::SummarizeCandidate;

    /// 2026-07-17T03:34:00Z = JST 12:34。
    fn ts(rfc3339: &str) -> DateTime<Utc> {
        DateTime::parse_from_rfc3339(rfc3339)
            .unwrap()
            .with_timezone(&Utc)
    }

    fn msg(user_id: u64, name: &str, content: &str, at: &str) -> MessageInfo {
        MessageInfo {
            message_id: serenity::all::MessageId::from(1),
            user_id: UserId::from(user_id),
            user_name: name.to_owned(),
            timestamp: ts(at),
            content: content.to_owned(),
        }
    }

    const MANAMI: u64 = 999;

    fn manami() -> UserId {
        UserId::from(MANAMI)
    }

    /// 人間の実会話。MIN_HUMAN_MESSAGES / MIN_HUMAN_CHARS を余裕で満たす。
    fn real_conversation() -> Vec<MessageInfo> {
        (0..8)
            .map(|i| {
                msg(
                    1,
                    "宇田",
                    "スピーカーを買い替えた話をしていて、Bluetooth の再接続が面倒だと感じている",
                    &format!("2026-07-17T03:{:02}:00Z", 30 + i),
                )
            })
            .collect()
    }

    #[test]
    fn skip_reason_rejects_too_few_messages() {
        // MIN_HUMAN_MESSAGES 未満なら、文字数が足りていても件数で弾く。
        // 各発言を十分長くして、弾かれる理由が「短すぎる」ではなく「少なすぎる」だと確かめる。
        let long = "あ".repeat(MIN_HUMAN_CHARS + 1);
        let messages: Vec<MessageInfo> = (0..MIN_HUMAN_MESSAGES.saturating_sub(1))
            .map(|i| {
                msg(
                    1,
                    "宇田",
                    &long,
                    &format!("2026-07-17T03:{:02}:00Z", 30 + i),
                )
            })
            .collect();
        assert_eq!(
            skip_reason(&messages, manami()),
            Some("人間の発言が少なすぎる")
        );
    }

    #[test]
    fn skip_reason_rejects_greetings_only() {
        // 件数は足りるが中身が短い、挨拶と相槌だけのセッション。
        let messages: Vec<MessageInfo> = (0..8)
            .map(|i| {
                msg(
                    1,
                    "宇田",
                    "そうだね",
                    &format!("2026-07-17T03:{:02}:00Z", 30 + i),
                )
            })
            .collect();
        assert_eq!(
            skip_reason(&messages, manami()),
            Some("人間の発言が短すぎる")
        );
    }

    #[test]
    fn skip_reason_rejects_manami_only() {
        // まなみの独り言しかないセッションは人間の発言 0 件。
        let messages: Vec<MessageInfo> = (0..8)
            .map(|i| {
                msg(
                    MANAMI,
                    "まなみ",
                    "そうなんだ！　それでね、こういう長い話をしていたことにするね",
                    &format!("2026-07-17T03:{:02}:00Z", 30 + i),
                )
            })
            .collect();
        assert_eq!(
            skip_reason(&messages, manami()),
            Some("人間の発言が少なすぎる")
        );
    }

    #[test]
    fn skip_reason_rejects_commands_only() {
        // コマンド実行だけのセッションは記憶に値しない。
        let messages: Vec<MessageInfo> = (0..8)
            .map(|i| {
                msg(
                    1,
                    "宇田",
                    "!dice 100d6 とにかく長い引数をつけて文字数だけは稼いでみることにする",
                    &format!("2026-07-17T03:{:02}:00Z", 30 + i),
                )
            })
            .collect();
        assert_eq!(
            skip_reason(&messages, manami()),
            Some("人間の発言が少なすぎる")
        );
    }

    #[test]
    fn skip_reason_accepts_real_conversation() {
        assert_eq!(skip_reason(&real_conversation(), manami()), None);
    }

    #[test]
    fn skip_reason_accepts_exact_boundary() {
        // ちょうど MIN_HUMAN_MESSAGES 件・MIN_HUMAN_CHARS 文字は通す(境界は含む)。
        let filler: String = "あ".repeat(MIN_HUMAN_CHARS / MIN_HUMAN_MESSAGES + 1);
        let messages: Vec<MessageInfo> = (0..MIN_HUMAN_MESSAGES)
            .map(|i| {
                msg(
                    1,
                    "宇田",
                    &filler,
                    &format!("2026-07-17T03:{:02}:00Z", 30 + i),
                )
            })
            .collect();
        assert_eq!(skip_reason(&messages, manami()), None);
    }

    fn candidate(name: &str) -> SummarizeCandidate {
        SummarizeCandidate {
            channel_id: "123".to_owned(),
            name: name.to_owned(),
            last_summarized_at: None,
            first_pending_at: "2026-07-17T03:00:00Z".to_owned(),
            last_message_at: "2026-07-17T03:00:00Z".to_owned(),
            pending_count: 1,
            last_summarized_message_id: None,
            first_pending_message_id: Some("1".to_owned()),
            last_pending_message_id: Some("1".to_owned()),
        }
    }

    #[test]
    fn channel_name_falls_back_to_id_when_blank() {
        // PUT /channel/summary のプレースホルダ INSERT が name='' の行を作りうる。
        assert_eq!(channel_name(&candidate("general")), "general");
        assert_eq!(channel_name(&candidate("")), "123");
        assert_eq!(channel_name(&candidate("   ")), "123");
    }

    #[test]
    fn build_title_names_the_channel() {
        assert_eq!(build_title("夢日記"), "#夢日記 の会話");
    }
}
