//! チャンネルの会話セッションを自動要約して memory 化する常駐タスク。
//!
//! IDLE_GAP 分発言がないか、未要約のログが SESSION_MESSAGE_LIMIT 件たまったときに発火する。

use std::sync::Arc;

use chrono::{DateTime, TimeDelta, Utc};
use dashmap::DashSet;
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

/// `last_summarized_at` が未設定のときに遡る上限。古いログを見過ぎないため
const MAX_LOOKBACK: TimeDelta = TimeDelta::hours(24);

/// 1 tick が LLM を叩く上限。
const MAX_CHANNELS_PER_TICK: usize = 3;
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

    // 直前に失敗したチャンネルは、次に候補へ現れたとき1回だけ見送る。累積でも指数でもない。
    let backoff: DashSet<ChannelId> = DashSet::new();

    let mut ticker = interval(TICK);
    ticker.set_missed_tick_behavior(MissedTickBehavior::Delay);
    loop {
        ticker.tick().await;
        tick_once(&bot, my_userid, &backoff).await;
    }
}

/// 1 tick 分の処理。候補を引いて先頭から順に要約する。
/// tick 同士は重ならない(このループの中で await するため)。
async fn tick_once(bot: &Bot, my_userid: UserId, backoff: &DashSet<ChannelId>) {
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

    for candidate in candidates.into_iter().take(MAX_CHANNELS_PER_TICK) {
        let Ok(channel_id) = candidate.channel_id.parse::<u64>().map(ChannelId::from) else {
            error!("summarizer: invalid channel_id: {}", candidate.channel_id);
            continue;
        };

        // 直前に失敗したチャンネルは、この登場を1回だけ見送る。
        if backoff.remove(&channel_id).is_some() {
            continue;
        }

        match summarize_channel(bot, my_userid, &candidate, now).await {
            Ok(outcome) => {
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
                backoff.insert(channel_id);
                error!("summarizer: failed on #{}: {e:?}", candidate.name);
            }
        }
    }
}

/// 1チャンネル分の未要約範囲を要約して記憶に落とす。
/// 進捗(`last_summarized_at`)を前進させるのは、記憶を登録できたときと、要約する価値が無いと判断したときだけ
pub async fn summarize_channel(
    bot: &Bot,
    my_userid: UserId,
    candidate: &udamanami_shared::SummarizeCandidate,
    now: DateTime<Utc>,
) -> anyhow::Result<Outcome> {
    let channel_id = ChannelId::from(candidate.channel_id.parse::<u64>()?);
    let from = resolve_from(candidate.last_summarized_at.as_deref(), now);

    let messages = bot
        .database
        .fetch_log_by_range(&channel_id, Some(from), None, MAX_MESSAGES_PER_RUN)
        .await?;
    // workers 側の from は閉区間(timestamp >= from)なので、境界の1件を落とす。
    // これをやらないと、前セッションの最終発言が毎回先頭に混ざる。
    let messages = drop_at_or_before(messages, from);

    let Some(last) = messages.last() else {
        return Ok(Outcome::Nothing);
    };
    // 進めるのは「実際に取れた最後の発言」まで。candidate.last_message_at にすると、
    // MAX_MESSAGES_PER_RUN で切られた分を要約せずに飛ばしてしまう。
    let session_end = last.timestamp;
    let session_start = messages[0].timestamp;

    if let Some(reason) = skip_reason(&messages, my_userid) {
        bot.database
            .set_channel_summarized(&channel_id, session_end)
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
            .set_channel_summarized(&channel_id, session_end)
            .await?;
        return Ok(Outcome::Skipped("モデルが記憶に値しないと判断した"));
    };

    // 出所(チャンネル名・会話日時)は本文と別の列に持たせる。夢日記の話を事実として語らせない文脈。
    let title = build_title(&channel_name);
    bot.database
        .create_summary_memory(&title, summary.trim(), &channel_name, session_start)
        .await?;
    bot.database
        .set_channel_summarized(&channel_id, session_end)
        .await?;

    Ok(Outcome::Summarized { title })
}

/// 通常 tick が待つ無音/件数の区切りを飛ばし、指定チャンネルを即座に候補化する(!summarize 用)。
///
/// idle_before を未来にして workers 側 HAVING の `MAX(timestamp) <= idle_before` を必ず真にし、
/// min_pending=1 で件数下限を落とす。未要約が 0 件なら None(= 無変動なら要約しない不変条件は保つ)。
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

/// 未要約範囲の開始時刻を決める。未設定なら直近 MAX_LOOKBACK まで遡る。
fn resolve_from(last_summarized_at: Option<&str>, now: DateTime<Utc>) -> DateTime<Utc> {
    last_summarized_at
        .and_then(|s| DateTime::parse_from_rfc3339(s).ok())
        .map_or(now - MAX_LOOKBACK, |t| t.with_timezone(&Utc))
}

/// `from` 以前のメッセージを落とす。workers 側の from が閉区間であることへの対処。
fn drop_at_or_before(messages: Vec<MessageInfo>, from: DateTime<Utc>) -> Vec<MessageInfo> {
    messages
        .into_iter()
        .filter(|m| m.timestamp > from)
        .collect()
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
    fn resolve_from_falls_back_to_lookback_when_unset() {
        let now = ts("2026-07-17T12:00:00Z");
        // 未設定なら直近 MAX_LOOKBACK まで遡る。過去ログ全部を要約しにいかない。
        assert_eq!(resolve_from(None, now), now - MAX_LOOKBACK);
        // 設定済みならその時刻から。
        assert_eq!(
            resolve_from(Some("2026-07-17T09:00:00Z"), now),
            ts("2026-07-17T09:00:00Z")
        );
        // 壊れた値は未設定と同じ扱いにして、会話を止めない。
        assert_eq!(resolve_from(Some("not a date"), now), now - MAX_LOOKBACK);
    }

    #[test]
    fn drop_at_or_before_excludes_the_boundary_message() {
        // workers 側の from は閉区間なので、from ちょうどの1件が返ってくる。
        // これを落とさないと前セッションの最終発言が毎回先頭に混ざる。
        let from = ts("2026-07-17T03:00:00Z");
        let messages = vec![
            msg(1, "宇田", "前セッションの最後", "2026-07-17T03:00:00Z"),
            msg(1, "宇田", "今セッションの最初", "2026-07-17T03:00:01Z"),
        ];
        let kept = drop_at_or_before(messages, from);
        assert_eq!(kept.len(), 1);
        assert_eq!(kept[0].content, "今セッションの最初");
    }

    #[test]
    fn skip_reason_rejects_too_few_messages() {
        // MIN_HUMAN_MESSAGES 未満なら、文字数が足りていても件数で弾く。
        // 各発言を十分長くして、弾かれる理由が「短すぎる」ではなく「少なすぎる」だと確かめる。
        let long = "あ".repeat(MIN_HUMAN_CHARS + 1);
        let messages: Vec<MessageInfo> = (0..MIN_HUMAN_MESSAGES.saturating_sub(1))
            .map(|i| msg(1, "宇田", &long, &format!("2026-07-17T03:{:02}:00Z", 30 + i)))
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
            first_pending_at: "2026-07-17T03:30:00Z".to_owned(),
            last_message_at: "2026-07-17T05:02:00Z".to_owned(),
            pending_count: 8,
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
