use crate::db::migrator::Migrator;
use sea_orm::prelude::*;
use sea_orm::Database;
use sea_orm_migration::migrator::MigratorTrait;
//use sea_orm_migration::SchemaManager;

pub mod migrator;

pub struct BotDatabase {
    db: DatabaseConnection,
}

impl BotDatabase {
    pub async fn new(path: &str) -> anyhow::Result<Self> {
        let db = Database::connect(format!("sqlite://{path}?mode=rwc")).await?;
        //let schema_manager = SchemaManager::new(&db);
        Migrator::refresh(&db).await?;

        Ok(Self { db })
    }

    /*
    pub async fn new_guild_message(&self, message: &MessageCreateEvent) -> anyhow::Result<()> {
        self.upsert_user(&message.author).await?;
        self.upsert_channel(&message.channel).await?;

        /*
        MessageEntity::insert(???)
            .exec(&self.db)
            .await?;
        */

        Ok(())
    }

    pub async fn update_guild_message(&self, edited_message: &MessageUpdateEvent) -> anyhow::Result<()> {

            /*
            message::Entity::update(???)
            .filter(Column::MessageId.eq(edited_message.id))
            .exec(&self.db)
            .await?;
            */

        Ok(())
    }

    pub async fn delete_guild_message(&self, message_id: i64) -> anyhow::Result<()> {
            /* */
            MessageEntity::delete(...)
            .filter(Column::MessageId.eq(message_id))
            .exec(&self.db)
            .await?;

        Ok(())
    }

    pub async fn fetch_log_by_count(&self, channel_id: i64, n: usize) -> anyhow::Result<Vec<MessageModel>> {
        MessageEntity::find()
            .filter(Column::ChannelId.eq(channel_id))
            .order_by_desc(Column::Timestamp)
            .limit(n as u64)
            .all(&self.db)
            .await
    }

    pub async fn fetch_log_by_duration(&self, channel_id: i64, duration: Duration) -> anyhow::Result<Vec<MessageModel>> {
        let since = Utc::now() - duration;

        MessageEntity::find()
            .filter(Column::ChannelId.eq(channel_id))
            .filter(Column::Timestamp.gte(since))
            .order_by_asc(Column::Timestamp)
            .all(&self.db)
            .await
    }

    pub async fn upsert_var<T>(&self, varname: &str, expr: T) -> anyhow::Result<()>
    where
        T: Serialize,
    {
        let value = serde_json::to_value(expr)?;

        // UPSERTのクエリ
        CalcvarEntity::insert(...)
            .on_conflict(...)
            .exec(&self.db)
            .await?;

        Ok(())
    }
    */
}
