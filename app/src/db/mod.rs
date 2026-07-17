//! workers (D1) API をバックエンドにした DB サービス層。
//!
//! serenity 型と shared DTO(文字列)の変換をここで行い、
//! HTTP 通信は [`WorkersApi`] に任せる。

use std::collections::HashMap;

use chrono::{DateTime, Utc};
use dashmap::DashMap;
use serenity::all::MessageId;
use serenity::model::{
    channel::Message,
    id::{ChannelId, UserId},
};
use udamanami_shared as dto;
use udamanami_shared::{
    GetMessages, MemoryDetail, MemoryListItem, MemorySearchResult, MessageOrder, UserProfile,
};

use crate::ai::ChatMessage;
use crate::calculator::{EvalContext, EvalResult};
use crate::db::workers_api::WorkersApi;

mod workers_api;

/// 期間指定なしの取得で使う実質無制限の件数。
const LIMIT_UNBOUNDED: usize = 1_000_000_000;

pub struct BotDatabase {
    api: WorkersApi,
}

impl BotDatabase {
    pub fn new(base_url: impl Into<String>, auth_token: impl Into<String>) -> Self {
        Self {
            api: WorkersApi::new(base_url, auth_token),
        }
    }

    pub async fn insert_guild_message(
        &self,
        message: &Message,
        user_name: &str,
        channel_name: &str,
    ) -> anyhow::Result<()> {
        self.upsert_user(&message.author.id, user_name).await?;
        self.upsert_channel(&message.channel_id, channel_name, message.thread.is_some())
            .await?;

        self.api
            .insert_messages(&[message_to_dto(message, &message.channel_id)])
            .await
    }

    pub async fn insert_many_guild_messages(
        &self,
        messages: &[Message],
        unique_users: HashMap<UserId, String>,
        channel_info: (ChannelId, String),
    ) -> anyhow::Result<()> {
        let (channel_id, channel_name) = channel_info;

        self.upsert_channel(&channel_id, &channel_name, false)
            .await?;

        for (user_id, user_name) in unique_users {
            self.upsert_user(&user_id, &user_name).await?;
        }

        let messages: Vec<dto::Message> = messages
            .iter()
            .map(|message| message_to_dto(message, &channel_id))
            .collect();

        self.api.insert_messages(&messages).await
    }

    pub async fn update_guild_message(&self, edited_message: &Message) -> anyhow::Result<()> {
        self.api
            .update_message(
                edited_message.id.get().to_string(),
                edited_message.content.clone(),
            )
            .await
    }

    pub async fn delete_guild_message(&self, message_id: &MessageId) -> anyhow::Result<()> {
        self.api.delete_message(message_id.get().to_string()).await
    }

    pub async fn upsert_user(&self, user_id: &UserId, user_name: &str) -> anyhow::Result<()> {
        self.api
            .upsert_user(user_id.get().to_string(), user_name.to_owned())
            .await
    }

    pub async fn set_user_room_pointer(
        &self,
        user_id: &UserId,
        username: &str,
        room_pointer: Option<ChannelId>,
    ) -> anyhow::Result<()> {
        self.api
            .set_room_pointer(
                user_id.get().to_string(),
                username.to_owned(),
                room_pointer.map(|c| c.get().to_string()),
            )
            .await
    }

    pub async fn fetch_user_room_pointer(
        &self,
        user_id: &UserId,
        default_pointer: ChannelId,
    ) -> anyhow::Result<ChannelId> {
        let room_pointer = self.api.get_room_pointer(user_id.get().to_string()).await?;

        Ok(room_pointer
            .and_then(|p| p.parse::<u64>().ok())
            .map_or(default_pointer, ChannelId::from))
    }

    /// 応答相手の人間プロフィールを取得する。未登録なら `None`。
    /// AI レイヤを serenity 非依存に保つため、生の Discord id 文字列で受ける。
    pub async fn fetch_user_profile(&self, user_id: &str) -> anyhow::Result<Option<UserProfile>> {
        self.api.get_user_profile(user_id.to_owned()).await
    }

    /// 応答相手の人間プロフィールを部分更新する。
    /// `None` のフィールドは据え置き、`Some(値)` はそのフィールドを全文置換する。
    pub async fn set_user_profile(
        &self,
        user_id: &str,
        calling_name: Option<String>,
        liked_topics: Option<String>,
        disliked_topics: Option<String>,
    ) -> anyhow::Result<()> {
        self.api
            .set_user_profile(&dto::SetUserProfile {
                user_id: user_id.to_owned(),
                calling_name,
                liked_topics,
                disliked_topics,
            })
            .await
    }

    pub async fn upsert_channel(
        &self,
        channel_id: &ChannelId,
        channel_name: &str,
        is_thread: bool,
    ) -> anyhow::Result<()> {
        self.api
            .upsert_channel(&dto::Channel {
                channel_id: channel_id.get().to_string(),
                is_thread,
                name: channel_name.to_owned(),
            })
            .await
    }

    /// チャンネルの自発反応設定を部分更新する。`None` のフィールドは据え置き。
    pub async fn set_channel_reply_setting(
        &self,
        channel_id: &ChannelId,
        reply_enabled: Option<bool>,
        reply_rate: Option<u32>,
    ) -> anyhow::Result<()> {
        self.api
            .set_channel_reply_setting(&dto::ChannelReplySetting {
                channel_id: channel_id.get().to_string(),
                reply_enabled,
                reply_rate,
            })
            .await
    }

    /// チャンネルの自発反応設定を取得する。未登録なら `None`。
    pub async fn fetch_channel_reply_setting(
        &self,
        channel_id: &ChannelId,
    ) -> anyhow::Result<Option<dto::ChannelReplySetting>> {
        self.api
            .get_channel_reply_setting(channel_id.get().to_string())
            .await
    }

    pub async fn fetch_oldest_message(
        &self,
        channel_id: &ChannelId,
    ) -> anyhow::Result<Option<MessageInfo>> {
        let messages = self
            .get_messages(channel_id, 1, MessageOrder::Asc, None, None)
            .await?;
        Ok(messages.into_iter().next())
    }

    pub async fn fetch_log_by_count(
        &self,
        channel_id: &ChannelId,
        n: usize,
    ) -> anyhow::Result<Vec<MessageInfo>> {
        let messages = self
            .get_messages(channel_id, n, MessageOrder::Desc, None, None)
            .await?;
        Ok(messages.into_iter().rev().collect())
    }

    pub async fn fetch_log_by_duration(
        &self,
        channel_id: &ChannelId,
        duration: chrono::Duration,
    ) -> anyhow::Result<Vec<MessageInfo>> {
        let since = Utc::now() - duration;
        self.get_messages(
            channel_id,
            LIMIT_UNBOUNDED,
            MessageOrder::Asc,
            Some(since),
            None,
        )
        .await
    }

    async fn get_messages(
        &self,
        channel_id: &ChannelId,
        limit: usize,
        order: MessageOrder,
        from: Option<DateTime<Utc>>,
        to: Option<DateTime<Utc>>,
    ) -> anyhow::Result<Vec<MessageInfo>> {
        let messages = self
            .api
            .get_messages(GetMessages {
                channel_id: channel_id.get().to_string(),
                limit,
                order: Some(order),
                from: from.map(|t| t.to_rfc3339()),
                to: to.map(|t| t.to_rfc3339()),
            })
            .await?;

        Ok(messages.into_iter().map(message_from_dto).collect())
    }

    pub async fn upsert_var(
        &self,
        varname: &str,
        x: EvalResult,
        author_id: UserId,
    ) -> anyhow::Result<()> {
        let value = serde_json::to_value(x)?;

        self.api
            .upsert_calc_var(&dto::CalcVar {
                var_name: varname.to_owned(),
                var_value: value.to_string(),
                user_id: author_id.get().to_string(),
            })
            .await
    }

    pub async fn retrieve_eval_context(&self) -> EvalContext {
        (self.api.get_all_calc_vars().await).map_or_else(
            |_| EvalContext::new(),
            |vars| {
                EvalContext::from_dashmap({
                    let dashmap = DashMap::new();
                    vars.into_iter().for_each(|var| {
                        if let Ok(value) = serde_json::from_str::<EvalResult>(&var.var_value) {
                            dashmap.insert(var.var_name, value);
                        }
                    });
                    dashmap
                })
            },
        )
    }

    pub async fn delete_var(&self, varname: &str) -> anyhow::Result<()> {
        self.api.delete_calc_var(varname.to_owned()).await
    }

    pub async fn list_var(&self) -> anyhow::Result<Vec<(String, String)>> {
        let vars = self.api.list_calc_vars().await?;

        Ok(vars
            .into_iter()
            .map(|var| {
                (
                    var.var_name,
                    var.username.unwrap_or_else(|| "[不明]".to_owned()),
                )
            })
            .collect())
    }

    pub async fn upsert_memory(&self, title: &str, content: &str) -> anyhow::Result<()> {
        self.api
            .create_memory(&dto::Memory {
                title: title.to_owned(),
                content: content.to_owned(),
            })
            .await
    }

    pub async fn update_memory(
        &self,
        memory_id: &str,
        title: &str,
        content: &str,
    ) -> anyhow::Result<()> {
        self.api
            .update_memory(&dto::UpdateMemory {
                memory_id: memory_id.to_owned(),
                title: title.to_owned(),
                content: content.to_owned(),
            })
            .await
    }

    pub async fn search_memory(
        &self,
        query: &str,
        limit: usize,
    ) -> anyhow::Result<Vec<MemorySearchResult>> {
        self.api.search_memory(query, limit).await
    }

    pub async fn delete_memory(&self, memory_id: &str) -> anyhow::Result<()> {
        self.api.delete_memory(memory_id.to_owned()).await
    }

    pub async fn get_memory(&self, memory_id: &str) -> anyhow::Result<MemoryDetail> {
        self.api.get_memory(memory_id.to_owned()).await
    }

    pub async fn list_memories(&self) -> anyhow::Result<Vec<MemoryListItem>> {
        self.api.list_memories().await
    }
}

fn message_to_dto(message: &Message, channel_id: &ChannelId) -> dto::Message {
    dto::Message {
        message_id: message.id.get().to_string(),
        channel_id: channel_id.get().to_string(),
        user_id: message.author.id.get().to_string(),
        content: message.content.clone(),
        timestamp: message.timestamp.to_utc().to_rfc3339(),
        username: None,
    }
}

fn message_from_dto(message: dto::Message) -> MessageInfo {
    MessageInfo {
        message_id: MessageId::from(message.message_id.parse::<u64>().unwrap_or(0)),
        user_id: UserId::from(message.user_id.parse::<u64>().unwrap_or(0)),
        user_name: message.username.unwrap_or_else(|| "Unknown".to_owned()),
        timestamp: DateTime::parse_from_rfc3339(&message.timestamp)
            .map_or_else(|_| Utc::now(), |t| t.with_timezone(&Utc)),
        content: message.content,
    }
}

#[derive(Clone, Debug)]
pub struct MessageInfo {
    pub message_id: MessageId,
    pub user_id: UserId,
    pub user_name: String,
    pub timestamp: DateTime<Utc>,
    pub content: String,
}

impl MessageInfo {
    pub fn to_chat_message(&self, my_userid: &UserId) -> ChatMessage {
        if self.user_id == *my_userid {
            ChatMessage::assistant(&self.content, self.timestamp)
        } else {
            ChatMessage::user(&self.user_name, &self.content, self.timestamp)
        }
    }
}
