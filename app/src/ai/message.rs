//! プロバイダ非依存のチャットメッセージ表現と、rig 形式への変換。

use chrono::{DateTime, FixedOffset, TimeDelta, Utc};
use rig::completion::Message as RigMessage;

/// メッセージの発言者。会話バッファと DB 由来のログで使う。
#[derive(Clone)]
pub enum Role {
    /// 人間の発言。`id` は Discord の user_id、`name` はそのときの表示名。
    User {
        id: u64,
        name: String,
    },
    Assistant,
}

impl Role {
    /// 同じ話者かどうか。Discord の user_id で判定する。
    fn same_speaker(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::User { id, .. }, Self::User { id: other, .. }) => id == other,
            (Self::Assistant, Self::Assistant) => true,
            _ => false,
        }
    }
}

/// プロバイダ非依存のチャットメッセージ。会話バッファと DB 由来のログで使う。
#[derive(Clone)]
pub struct ChatMessage {
    pub role: Role,
    pub content: String,
    pub timestamp: DateTime<Utc>,
}

impl ChatMessage {
    /// ユーザー発言。`user_id` は Discord の user_id。
    pub fn user(user_id: u64, user_name: &str, message: &str, timestamp: DateTime<Utc>) -> Self {
        Self {
            role: Role::User {
                id: user_id,
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

    /// 同じ話者の続きは、gap以下の間隔で投稿された場合、1つの発言として扱う。
    pub(crate) fn can_merge(&self, next: &Self, gap: TimeDelta) -> bool {
        self.role.same_speaker(&next.role) && next.timestamp - self.timestamp <= gap
    }

    /// 連投を本文へ繋げる。時刻はかたまりの開始時刻のまま据え置く。
    pub(crate) fn merge(&mut self, next: &Self) {
        self.content.push('\n');
        self.content.push_str(&next.content);
    }

    /// rig のメッセージ形式へ変換する。本文の先頭に発言時刻と話者名を付与する。
    pub(crate) fn to_rig(&self) -> RigMessage {
        let stamp = stamp(&self.timestamp);
        match &self.role {
            Role::User { name, .. } => {
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

    /// 宇田の user_id。テストの中で同一性の基準として使う。
    const UDA: u64 = 1;
    /// うさみむの user_id。
    const USAMIMU: u64 = 2;

    #[test]
    fn chat_message_roles() {
        // ユーザー発言は user_id と話者名を Role に保持し、content は本文そのもの。
        // 話者名・時刻の付与は LLM 送信時（to_rig）に行う。
        let user_msg = ChatMessage::user(UDA, "宇田", "やあ", fixed_ts());
        match &user_msg.role {
            Role::User { id, name } => {
                assert_eq!(*id, UDA);
                assert_eq!(name.as_str(), "宇田");
            }
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

    /// 固定時刻から `minutes` 分進めた時刻。
    fn after(minutes: i64) -> DateTime<Utc> {
        fixed_ts() + TimeDelta::minutes(minutes)
    }

    #[test]
    fn can_merge_only_within_the_same_speaker_and_gap() {
        let gap = TimeDelta::minutes(3);
        let uda = ChatMessage::user(UDA, "宇田", "やあ", fixed_ts());

        // 同じ話者で間隔が gap 以内なら繋げる。境界も含む。
        assert!(uda.can_merge(&ChatMessage::user(UDA, "宇田", "元気？", after(1)), gap));
        assert!(uda.can_merge(&ChatMessage::user(UDA, "宇田", "元気？", after(3)), gap));

        // gap を超えたら別の発言。
        assert!(!uda.can_merge(&ChatMessage::user(UDA, "宇田", "元気？", after(4)), gap));

        // 話者が違えば繋げない。
        assert!(!uda.can_merge(
            &ChatMessage::user(USAMIMU, "うさみむ", "やっほー", after(1)),
            gap
        ));
        assert!(!uda.can_merge(&ChatMessage::assistant("やっほー", after(1)), gap));

        // まなみの連投どうしは繋げる。
        let manami = ChatMessage::assistant("やっほー", fixed_ts());
        assert!(manami.can_merge(&ChatMessage::assistant("元気？", after(1)), gap));
    }

    #[test]
    fn can_merge_follows_the_id_not_the_display_name() {
        let gap = TimeDelta::minutes(3);
        let uda = ChatMessage::user(UDA, "宇田", "やあ", fixed_ts());

        // 途中で表示名を変えても同じ人。
        assert!(uda.can_merge(&ChatMessage::user(UDA, "うだ", "元気？", after(1)), gap));

        // 同じ表示名を名乗る別人は繋げない。
        assert!(!uda.can_merge(&ChatMessage::user(USAMIMU, "宇田", "偽物", after(1)), gap));
    }

    #[test]
    fn merge_appends_body_and_keeps_the_first_timestamp() {
        let mut uda = ChatMessage::user(UDA, "宇田", "やあ", fixed_ts());
        uda.merge(&ChatMessage::user(UDA, "宇田", "元気？", after(1)));

        assert_eq!(uda.content, "やあ\n元気？");
        // 時刻はかたまりの開始時刻。連投の途中で先頭の時刻が動くと、
        // ログ上で発言の順序と時刻がずれる。
        assert_eq!(uda.timestamp, fixed_ts());
    }

    #[test]
    fn to_rig_bakes_timestamp_and_name() {
        // ユーザー発言: [時刻] 名前: 本文
        let user_msg = ChatMessage::user(UDA, "宇田", "やあ", fixed_ts());
        assert!(matches!(user_msg.to_rig(), RigMessage::User { .. }));
        let user_text = format!("{:?}", user_msg.to_rig());
        assert!(
            user_text.contains("[2026-07-17 12:34] 宇田: やあ"),
            "unexpected user content: {user_text}"
        );
        // user_id はモデルへ渡さない。時刻や本文の数字と紛れない ID で確かめる。
        let distinct = ChatMessage::user(999_999_999, "宇田", "やあ", fixed_ts());
        let distinct_text = format!("{:?}", distinct.to_rig());
        assert!(
            !distinct_text.contains("999999999"),
            "user_id leaked into the prompt: {distinct_text}"
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
