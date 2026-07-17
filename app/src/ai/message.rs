//! プロバイダ非依存のチャットメッセージ表現と、rig 形式への変換。

use chrono::{DateTime, FixedOffset, Utc};
use rig::completion::Message as RigMessage;

/// メッセージの発言者。会話バッファと DB 由来のログで使う。
#[derive(Clone)]
pub enum Role {
    User { name: String },
    Assistant,
}

/// プロバイダ非依存のチャットメッセージ。会話バッファと DB 由来のログで使う。
#[derive(Clone)]
pub struct ChatMessage {
    pub role: Role,
    pub content: String,
    pub timestamp: DateTime<Utc>,
}

impl ChatMessage {
    /// ユーザー発言。
    pub fn user(user_name: &str, message: &str, timestamp: DateTime<Utc>) -> Self {
        Self {
            role: Role::User {
                name: user_name.to_owned(),
            },
            content: message.to_owned(),
            timestamp,
        }
    }

    /// まなみ（アシスタント）の発言。
    pub fn assistant(message: &str, timestamp: DateTime<Utc>) -> Self {
        Self {
            role: Role::Assistant,
            content: message.to_owned(),
            timestamp,
        }
    }

    /// rig のメッセージ形式へ変換する。本文の先頭に発言時刻と話者名を付与する。
    pub(crate) fn to_rig(&self) -> RigMessage {
        let stamp = stamp(&self.timestamp);
        match &self.role {
            Role::User { name } => {
                RigMessage::user(format!("{} {}: {}", stamp, name, self.content))
            }
            Role::Assistant => RigMessage::assistant(format!("{} {}", stamp, self.content)),
        }
    }
}

/// UTC 時刻を JST・分精度の `YYYY-MM-DD HH:MM` へ整形する。
pub fn stamp_jst(ts: &DateTime<Utc>) -> String {
    let jst = FixedOffset::east_opt(9 * 3600).expect("valid JST offset");
    ts.with_timezone(&jst).format("%Y-%m-%d %H:%M").to_string()
}

/// UTC 時刻を JST・分精度の `[YYYY-MM-DD HH:MM]` へ整形する。
fn stamp(ts: &DateTime<Utc>) -> String {
    format!("[{}]", stamp_jst(ts))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// テスト用に固定の UTC 時刻を作る。2026-07-17T03:34:00Z = JST 12:34。
    fn fixed_ts() -> DateTime<Utc> {
        DateTime::parse_from_rfc3339("2026-07-17T03:34:00Z")
            .unwrap()
            .with_timezone(&Utc)
    }

    #[test]
    fn chat_message_roles() {
        // ユーザー発言は話者名を Role に保持し、content は本文そのもの。
        // 話者名・時刻の付与は LLM 送信時（to_rig）に行う。
        let user_msg = ChatMessage::user("宇田", "やあ", fixed_ts());
        match &user_msg.role {
            Role::User { name } => assert_eq!(name.as_str(), "宇田"),
            Role::Assistant => panic!("expected a user role"),
        }
        assert_eq!(user_msg.content, "やあ");

        // まなみ（アシスタント）の発言は名前を持たず、content は本文そのもの。
        let assistant_msg = ChatMessage::assistant("やっほー", fixed_ts());
        assert!(matches!(assistant_msg.role, Role::Assistant));
        assert_eq!(assistant_msg.content, "やっほー");
    }

    #[test]
    fn stamp_converts_utc_to_jst_minute() {
        // UTC 03:34 は JST（+09:00）で 12:34。分精度で焼き込む。
        assert_eq!(stamp(&fixed_ts()), "[2026-07-17 12:34]");
    }

    #[test]
    fn to_rig_bakes_timestamp_and_name() {
        // ユーザー発言: [時刻] 名前: 本文
        let user_msg = ChatMessage::user("宇田", "やあ", fixed_ts());
        assert!(matches!(user_msg.to_rig(), RigMessage::User { .. }));
        let user_text = format!("{:?}", user_msg.to_rig());
        assert!(
            user_text.contains("[2026-07-17 12:34] 宇田: やあ"),
            "unexpected user content: {user_text}"
        );

        // まなみの発言: [時刻] 本文（名前は付けない）
        let assistant_msg = ChatMessage::assistant("やっほー", fixed_ts());
        assert!(matches!(
            assistant_msg.to_rig(),
            RigMessage::Assistant { .. }
        ));
        let assistant_text = format!("{:?}", assistant_msg.to_rig());
        assert!(
            assistant_text.contains("[2026-07-17 12:34] やっほー"),
            "unexpected assistant content: {assistant_text}"
        );
    }
}
