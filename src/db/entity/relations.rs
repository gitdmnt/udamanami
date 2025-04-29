use crate::db::entity::{channel, message, user};
use sea_orm::entity::prelude::*;

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
