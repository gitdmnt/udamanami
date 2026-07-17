//! botと workersで共有するDTO型.

use serde::{Deserialize, Serialize};

pub type MessageId = String;
pub type ChannelId = String;
pub type UserId = String;
pub type MemoryId = String;
pub type ChunkId = String;

// ---------------- メッセージ ----------------

#[derive(Debug, Deserialize, Serialize)]
pub struct Message {
    pub message_id: MessageId,
    pub channel_id: ChannelId,
    pub user_id: UserId,
    pub content: String,
    pub timestamp: String,
    /// 取得時のみ user テーブルとの JOIN で埋まる。挿入時は不要。
    #[serde(default)]
    pub username: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct UpdateMessage {
    pub message_id: MessageId,
    pub content: String,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct DeleteMessage {
    pub message_id: MessageId,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct GetMessages {
    pub channel_id: ChannelId,
    pub limit: usize,
    pub order: Option<MessageOrder>,
    pub from: Option<String>, // chrono::DateTime<chrono::Utc>
    pub to: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum MessageOrder {
    Asc,
    Desc,
}

// ---------------- チャンネル ----------------

#[derive(Debug, Deserialize, Serialize)]
pub struct Channel {
    pub channel_id: ChannelId,
    pub is_thread: bool,
    pub name: String,
}

/// チャンネルごとの自発反応設定。`None` は「未設定 → 実行時デフォルト」を表す。
/// 頻繁な [`Channel`] upsert 経路とは分離し、name/is_thread を上書きせず部分更新する。
#[derive(Debug, Deserialize, Serialize)]
pub struct ChannelReplySetting {
    pub channel_id: ChannelId,
    /// 自発反応を許可するか。
    pub reply_enabled: Option<bool>,
    /// 自発反応する割合(%)。0..=100。
    pub reply_rate: Option<u32>,
}

// ---------------- ユーザー ----------------

#[derive(Debug, Deserialize, Serialize)]
pub struct User {
    pub user_id: UserId,
    pub username: String,
    pub room_pointer: Option<ChannelId>,
}

/// まなみが応答相手について学習した人間プロフィール。
#[derive(Debug, Deserialize, Serialize)]
pub struct UserProfile {
    pub user_id: UserId,
    pub username: String,
    /// 希望する呼び名。
    pub calling_name: Option<String>,
    /// 好きな話題。
    pub liked_topics: Option<String>,
    /// 嫌いな話題。
    pub disliked_topics: Option<String>,
}

/// プロフィールの部分更新リクエスト。
#[derive(Debug, Deserialize, Serialize)]
pub struct SetUserProfile {
    pub user_id: UserId,
    pub calling_name: Option<String>,
    pub liked_topics: Option<String>,
    pub disliked_topics: Option<String>,
}

impl UserProfile {
    /// 応答相手のプロフィールをシステムプロンプト末尾へ差し込むブロックに整形する。
    /// `None` または空白のみのフィールドは行ごと省く。表示すべき項目が皆無なら空文字列を返す。
    pub fn to_prompt(&self) -> String {
        let lines: Vec<String> = [
            profile_line("名前", Some(self.username.as_str())),
            profile_line("希望する呼び名", self.calling_name.as_deref()),
            profile_line("好きな話題", self.liked_topics.as_deref()),
            profile_line("嫌いな話題", self.disliked_topics.as_deref()),
        ]
        .into_iter()
        .flatten()
        .collect();

        if lines.is_empty() {
            String::new()
        } else {
            format!(
                "## いま応答している相手のプロフィール\n{}",
                lines.join("\n")
            )
        }
    }
}

/// 値があり、trim 後に非空なら `- ラベル: 値` の 1 行を返す。それ以外は `None`。
fn profile_line(label: &str, value: Option<&str>) -> Option<String> {
    let trimmed = value?.trim();
    if trimmed.is_empty() {
        None
    } else {
        Some(format!("- {label}: {trimmed}"))
    }
}

// ---------------- calc var ----------------

#[derive(Debug, Deserialize, Serialize)]
pub struct CalcVar {
    pub var_name: String,
    pub var_value: String,
    pub user_id: UserId,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct DeleteCalcVar {
    pub var_name: String,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct CalcVarWithUsername {
    pub var_name: String,
    pub username: Option<String>,
}

// ---------------- memory ----------------

#[derive(Debug, Deserialize, Serialize)]
pub struct Memory {
    pub title: String,
    pub content: String,
}

/// 既存メモリ1件を新しい内容で置き換える。chunk とベクトルを作り直す。
#[derive(Debug, Deserialize, Serialize)]
pub struct UpdateMemory {
    pub memory_id: MemoryId,
    pub title: String,
    pub content: String,
}

/// 意味検索の結果1件。Vectorize の類似度 score を付与して返す。
#[derive(Debug, Deserialize, Serialize)]
pub struct MemorySearchResult {
    pub chunk_id: ChunkId,
    pub memory_id: MemoryId,
    pub chunk_index: i64,
    pub content: String,
    pub title: String,
    pub timestamp: String,
    pub score: f64,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct MemoryListItem {
    pub memory_id: MemoryId,
    pub title: String,
    pub timestamp: String,
}

/// メモリ1件の全文。chunk を連結して本文を復元したもの。
#[derive(Debug, Deserialize, Serialize)]
pub struct MemoryDetail {
    pub memory_id: MemoryId,
    pub title: String,
    pub timestamp: String,
    pub content: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn profile(
        username: &str,
        calling_name: Option<&str>,
        liked: Option<&str>,
        disliked: Option<&str>,
    ) -> UserProfile {
        UserProfile {
            user_id: "1".to_owned(),
            username: username.to_owned(),
            calling_name: calling_name.map(str::to_owned),
            liked_topics: liked.map(str::to_owned),
            disliked_topics: disliked.map(str::to_owned),
        }
    }

    #[test]
    fn to_prompt_includes_all_present_fields() {
        let block = profile(
            "うさみむ",
            Some("おねえちゃん"),
            Some("手芸"),
            Some("ホラー"),
        )
        .to_prompt();
        assert!(block.contains("## いま応答している相手のプロフィール"));
        assert!(block.contains("- 名前: うさみむ"));
        assert!(block.contains("- 希望する呼び名: おねえちゃん"));
        assert!(block.contains("- 好きな話題: 手芸"));
        assert!(block.contains("- 嫌いな話題: ホラー"));
    }

    #[test]
    fn to_prompt_skips_missing_fields_but_keeps_name() {
        let block = profile("だれか", None, None, None).to_prompt();
        assert!(block.contains("- 名前: だれか"));
        assert!(!block.contains("希望する呼び名"));
        assert!(!block.contains("好きな話題"));
        assert!(!block.contains("嫌いな話題"));
    }

    #[test]
    fn to_prompt_treats_empty_and_whitespace_as_absent() {
        // Some("") と空白のみは学習済みとみなさず行を出さない。
        let block = profile("ゲスト", Some(""), Some("   "), None).to_prompt();
        assert!(block.contains("- 名前: ゲスト"));
        assert!(!block.contains("希望する呼び名"));
        assert!(!block.contains("好きな話題"));
    }

    #[test]
    fn to_prompt_returns_empty_when_nothing_to_show() {
        // 名前すら空なら、注入すべきブロックは無い。
        let block = profile("  ", Some(""), None, None).to_prompt();
        assert!(block.is_empty());
    }
}
