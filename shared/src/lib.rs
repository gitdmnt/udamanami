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

// ---------------- ユーザー ----------------

#[derive(Debug, Deserialize, Serialize)]
pub struct User {
    pub user_id: UserId,
    pub username: String,
    pub room_pointer: Option<ChannelId>,
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
