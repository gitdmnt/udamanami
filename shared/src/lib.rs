//! botと workersで共有するDTO型.

use serde::{Deserialize, Serialize};

pub type MessageId = String;
pub type ChannelId = String;
pub type UserId = String;

// ---------------- メッセージ ----------------

#[derive(Debug, Deserialize, Serialize)]
pub struct Message {
    pub message_id: MessageId,
    pub channel_id: ChannelId,
    pub user_id: UserId,
    pub content: String,
    pub timestamp: String,
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
