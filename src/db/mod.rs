use std::collections::HashMap;

use crate::ai::GeminiContent;
use crate::calculator::EvalContext;
use crate::db::migrator::Migrator;
use chrono::DateTime;
use chrono::Utc;
use dashmap::DashMap;
use sea_orm::ConnectOptions;
use sea_orm::{prelude::*, sea_query::OnConflict, ActiveValue, Database, QueryOrder, QuerySelect};
use sea_orm_migration::migrator::MigratorTrait;
use serenity::all::MessageId;
use serenity::model::{
    channel::Message,
    id::{ChannelId, UserId},
};

use crate::calculator::EvalResult;
use crate::db::entity::*;

pub mod entity;
pub mod migrator;

pub struct BotDatabase {
    db: DatabaseConnection,
}

impl BotDatabase {
    pub async fn new(path: &str) -> anyhow::Result<Self> {
        let mut opt: ConnectOptions = format!("sqlite://{path}?mode=rwc").into();
        opt.sqlx_logging(false);
        let db = Database::connect(opt).await?;
        Migrator::up(&db, None).await?;

        Ok(Self { db })
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

        let message_model = message::ActiveModel {
            message_id: ActiveValue::Set(message.id.get() as i64),
            channel_id: ActiveValue::Set(message.channel_id.get() as i64),
            user_id: ActiveValue::Set(message.author.id.get() as i64),
            timestamp: ActiveValue::Set(message.timestamp.to_utc()),
            content: ActiveValue::Set(message.content.clone()),
        };

        message_model.insert(&self.db).await?;
        Ok(())
    }

    pub async fn insert_many_guild_messages(
        &self,
        messages: &[Message],
        unique_users: HashMap<UserId, String>,
        channel_info: (ChannelId, String),
    ) -> anyhow::Result<()> {
        let channel_id = channel_info.0;
        let channel_name = channel_info.1;

        self.upsert_channel(&channel_id, &channel_name, false)
            .await?;

        for (user_id, user_name) in unique_users {
            self.upsert_user(&user_id, &user_name).await?;
        }

        let message_models: Vec<message::ActiveModel> = messages
            .iter()
            .map(|message| message::ActiveModel {
                message_id: ActiveValue::Set(message.id.get() as i64),
                channel_id: ActiveValue::Set(channel_id.get() as i64),
                user_id: ActiveValue::Set(message.author.id.get() as i64),
                timestamp: ActiveValue::Set(message.timestamp.to_utc()),
                content: ActiveValue::Set(message.content.clone()),
            })
            .collect();

        message::Entity::insert_many(message_models)
            .on_conflict(
                OnConflict::columns([message::Column::MessageId])
                    .update_columns([message::Column::Content])
                    .to_owned(),
            )
            .exec(&self.db)
            .await?;

        Ok(())
    }

    pub async fn update_guild_message(&self, edited_message: &Message) -> anyhow::Result<()> {
        let message_model = message::ActiveModel {
            message_id: ActiveValue::Set(edited_message.id.get() as i64),
            content: ActiveValue::Set(edited_message.content.clone()),
            ..Default::default()
        };

        message_model.update(&self.db).await?;
        Ok(())
    }

    pub async fn delete_guild_message(&self, message_id: &MessageId) -> anyhow::Result<()> {
        let message_model = message::ActiveModel {
            message_id: ActiveValue::Set(message_id.get() as i64),
            ..Default::default()
        };
        message_model.delete(&self.db).await?;
        Ok(())
    }

    pub async fn upsert_user(&self, user_id: &UserId, user_name: &str) -> anyhow::Result<()> {
        let user_model = user::ActiveModel {
            user_id: ActiveValue::Set(user_id.get() as i64),
            username: ActiveValue::Set(user_name.to_owned()),
            ..Default::default()
        };

        user::Entity::insert(user_model)
            .on_conflict(
                OnConflict::columns([user::Column::UserId])
                    .update_columns([user::Column::Username])
                    .to_owned(),
            )
            .exec(&self.db)
            .await?;
        Ok(())
    }

    pub async fn set_user_room_pointer(
        &self,
        user_id: &UserId,
        room_pointer: Option<ChannelId>,
    ) -> anyhow::Result<()> {
        let user_model = user::ActiveModel {
            user_id: ActiveValue::Set(user_id.get() as i64),
            room_pointer: ActiveValue::Set(room_pointer.map(|c| c.get() as i64)),
            ..Default::default()
        };

        user::Entity::insert(user_model)
            .on_conflict(
                OnConflict::columns([user::Column::UserId])
                    .update_columns([user::Column::RoomPointer])
                    .to_owned(),
            )
            .exec(&self.db)
            .await?;
        Ok(())
    }

    pub async fn fetch_user_room_pointer(
        &self,
        user_id: &UserId,
    ) -> anyhow::Result<Option<ChannelId>> {
        let user_model = user::Entity::find()
            .filter(user::Column::UserId.eq(user_id.get() as i64))
            .one(&self.db)
            .await?;

        Ok(user_model.and_then(|u| u.room_pointer.map(|p| ChannelId::from(p as u64))))
    }

    pub async fn upsert_channel(
        &self,
        channel_id: &ChannelId,
        channel_name: &str,
        is_thread: bool,
    ) -> anyhow::Result<()> {
        let channel_model = channel::ActiveModel {
            channel_id: ActiveValue::Set(channel_id.get() as i64),
            is_thread: ActiveValue::Set(is_thread),
            name: ActiveValue::Set(channel_name.to_owned()),
        };

        channel::Entity::insert(channel_model)
            .on_conflict(
                OnConflict::columns([channel::Column::ChannelId])
                    .update_columns([channel::Column::Name, channel::Column::IsThread])
                    .to_owned(),
            )
            .exec(&self.db)
            .await?;
        Ok(())
    }

    pub async fn fetch_oldest_message(
        &self,
        channel_id: &ChannelId,
    ) -> anyhow::Result<Option<MessageInfo>> {
        let messages: Vec<(message::Model, Option<user::Model>)> = message::Entity::find()
            .filter(message::Column::ChannelId.eq(channel_id.get() as i64))
            .order_by_asc(message::Column::Timestamp)
            .limit(1)
            .find_also_related(user::Entity)
            .all(&self.db)
            .await?;

        if messages.is_empty() {
            return Ok(None);
        }

        let message = Self::query_result_to_message(messages)[0].clone();
        Ok(Some(message))
    }

    pub async fn fetch_log_by_count(
        &self,
        channel_id: &ChannelId,
        n: usize,
    ) -> anyhow::Result<Vec<MessageInfo>> {
        let messages: Vec<(message::Model, Option<user::Model>)> = message::Entity::find()
            .filter(message::Column::ChannelId.eq(channel_id.get() as i64))
            .order_by_desc(message::Column::Timestamp)
            .limit(n as u64)
            .find_also_related(user::Entity)
            .all(&self.db)
            .await?;

        Ok(Self::query_result_to_message(messages)
            .into_iter()
            .rev()
            .collect())
    }

    pub async fn fetch_log_by_duration(
        &self,
        channel_id: &ChannelId,
        duration: chrono::Duration,
    ) -> anyhow::Result<Vec<MessageInfo>> {
        let since = Utc::now() - duration;

        let messages: Vec<(message::Model, Option<user::Model>)> = message::Entity::find()
            .filter(message::Column::ChannelId.eq(channel_id.get() as i64))
            .filter(message::Column::Timestamp.gte(since))
            .order_by_asc(message::Column::Timestamp)
            .find_also_related(user::Entity)
            .all(&self.db)
            .await?;

        Ok(Self::query_result_to_message(messages)
            .into_iter()
            .collect())
    }

    pub async fn fetch_log_until_gap(
        &self,
        channel_id: &ChannelId,
        gap: chrono::Duration,
    ) -> anyhow::Result<Vec<MessageInfo>> {
        /*
        0. 「遡行開始点」を現在時刻に設定
        1. 「遡行開始点」から gap 分のメッセージを取得、result に追加
        2. 取得したメッセージが空でなければ、最初のメッセージの timestamp を「遡行開始点」に設定し1に戻る、さもなくば終了
        */

        // newer-first
        let mut messages = vec![];

        let mut since = Utc::now();
        loop {
            let mut chunk: Vec<(message::Model, Option<user::Model>)> = message::Entity::find()
                .filter(message::Column::ChannelId.eq(channel_id.get() as i64))
                .filter(message::Column::Timestamp.lt(since - gap))
                .order_by_desc(message::Column::Timestamp)
                .find_also_related(user::Entity)
                .all(&self.db)
                .await?;

            if chunk.is_empty() {
                break;
            }

            messages.append(&mut chunk);
            since = messages.last().unwrap().0.timestamp;
        }

        Ok(Self::query_result_to_message(messages)
            .into_iter()
            .rev()
            .collect())
    }

    fn query_result_to_message(
        messages: Vec<(message::Model, Option<user::Model>)>,
    ) -> Vec<MessageInfo> {
        messages
            .into_iter()
            .map(|(message, user)| {
                let message_id = MessageId::from(message.message_id as u64);
                let content = message.content.clone();
                let timestamp = message.timestamp;
                let user_id = UserId::from(user.as_ref().map_or(0, |u| u.user_id) as u64);
                let user_name = user
                    .as_ref()
                    .map_or("Unknown".to_owned(), |u| u.username.clone());
                MessageInfo {
                    message_id,
                    user_id,
                    user_name,
                    timestamp,
                    content,
                }
            })
            .collect()
    }

    pub async fn upsert_var(
        &self,
        varname: &str,
        x: EvalResult,
        author_id: UserId,
    ) -> anyhow::Result<()> {
        let value = serde_json::to_value(x)?;

        let var_model = calc_var::ActiveModel {
            var_name: ActiveValue::Set(varname.to_owned()),
            var_value: ActiveValue::Set(value.to_string()),
            user_id: ActiveValue::Set(author_id.get() as i64),
        };

        calc_var::Entity::insert(var_model)
            .on_conflict(
                OnConflict::columns([calc_var::Column::VarName])
                    .update_columns([calc_var::Column::VarValue])
                    .to_owned(),
            )
            .exec(&self.db)
            .await?;

        Ok(())
    }

    pub async fn retrieve_eval_context(&self) -> EvalContext {
        (calc_var::Entity::find().all(&self.db).await).map_or(EvalContext::new(), |models| {
            EvalContext::from_dashmap({
                let dashmap = DashMap::new();
                models.into_iter().for_each(|model| {
                    if let Ok(value) = serde_json::from_str::<EvalResult>(&model.var_value) {
                        dashmap.insert(model.var_name, value);
                    }
                });
                dashmap
            })
        })
    }

    pub async fn delete_var(&self, varname: &str) -> anyhow::Result<()> {
        let var_model = calc_var::ActiveModel {
            var_name: ActiveValue::Set(varname.to_owned()),
            ..Default::default()
        };

        var_model.delete(&self.db).await?;
        Ok(())
    }

    pub async fn list_var(&self) -> anyhow::Result<Vec<(String, String)>> {
        let vars = calc_var::Entity::find()
            .find_also_related(user::Entity)
            .order_by_asc(user::Column::Username)
            .all(&self.db)
            .await?;

        Ok(vars
            .into_iter()
            .map(|(var, user)| {
                (
                    var.var_name,
                    user.map_or("[不明]".to_owned(), |u| u.username),
                )
            })
            .collect())
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
    pub fn gemini_content(&self, my_userid: &UserId) -> GeminiContent {
        if self.user_id == *my_userid {
            GeminiContent::model(&self.content)
        } else {
            GeminiContent::user(&self.user_name, &self.content)
        }
    }
}

/// message.user_id -> user.id
impl Related<user::Entity> for message::Entity {
    fn to() -> RelationDef {
        Self::belongs_to(user::Entity)
            .from(message::Column::UserId)
            .to(user::Column::UserId)
            .into()
    }

    fn via() -> Option<RelationDef> {
        None
    }
}

/// message.channel_id -> channel.id
impl Related<channel::Entity> for message::Entity {
    fn to() -> RelationDef {
        Self::belongs_to(channel::Entity)
            .from(message::Column::ChannelId)
            .to(channel::Column::ChannelId)
            .into()
    }

    fn via() -> Option<RelationDef> {
        None
    }
}

impl Related<user::Entity> for calc_var::Entity {
    fn to() -> RelationDef {
        Self::belongs_to(user::Entity)
            .from(calc_var::Column::UserId)
            .to(user::Column::UserId)
            .into()
    }

    fn via() -> Option<RelationDef> {
        None
    }
}
