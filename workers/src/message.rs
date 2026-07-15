//! メッセージログのCRUD
use crate::channel::ChannelId;
use crate::user::UserId;
use serde::{Deserialize, Serialize};
use worker::*;

pub type MessageId = String;

#[derive(Debug, Deserialize, Serialize)]
struct Message {
    message_id: MessageId,
    channel_id: ChannelId,
    user_id: UserId,
    content: String,
    timestamp: String,
}

#[derive(Debug, Deserialize, Serialize)]
struct UpdateMessage {
    message_id: MessageId,
    content: String,
}

#[derive(Debug, Deserialize, Serialize)]
struct DeleteMessage {
    message_id: MessageId,
}

pub async fn insert_messages(mut req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let body = req.text().await?;
    let Ok(messages) = serde_json::from_str::<Vec<Message>>(&body) else {
        return Response::error("Failed to parse request body", 400);
    };

    let d1 = ctx.env.d1("DB")?;

    let stmts: Vec<D1PreparedStatement> = messages
        .into_iter()
        .map(|message| {
            d1.prepare(
                "INSERT INTO message (message_id, channel_id, user_id, timestamp, content)
                 VALUES (?, ?, ?, ?, ?)
ON CONFLICT (message_id) DO UPDATE SET content = excluded.content;
",
            )
            .bind(&[
                message.message_id.into(),
                message.channel_id.into(),
                message.user_id.into(),
                message.timestamp.into(),
                message.content.into(),
            ])
        })
        .collect::<Result<Vec<_>>>()?;

    let _ = d1.batch(stmts).await?;
    Response::ok("Messages inserted successfully")
}

pub async fn update_message(mut req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let body = req.text().await?;
    let Ok(message) = serde_json::from_str::<UpdateMessage>(&body) else {
        return Response::error("Failed to parse request body", 400);
    };

    let d1 = ctx.env.d1("DB")?;

    let _ = d1
        .prepare("UPDATE message SET content = ? WHERE message_id = ?")
        .bind(&[message.content.into(), message.message_id.into()])?
        .run()
        .await?;

    Response::ok("Message updated successfully")
}

pub async fn delete_message(mut req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let body = req.text().await?;
    let Ok(message) = serde_json::from_str::<DeleteMessage>(&body) else {
        return Response::error("Failed to parse request body", 400);
    };

    let d1 = ctx.env.d1("DB")?;

    let _ = d1
        .prepare("DELETE FROM message WHERE message_id = ?1")
        .bind(&[message.message_id.into()])?
        .run()
        .await?;

    Response::ok("Message deleted successfully")
}

/// 最古のメッセージn件を取得する
#[derive(Debug, Deserialize, Serialize)]
pub struct GetMessagesRequest {
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

pub async fn get_messages(mut req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let body = req.text().await?;
    let Ok(request) = serde_json::from_str::<GetMessagesRequest>(&body) else {
        return Response::error("Failed to parse request body", 400);
    };

    let d1 = ctx.env.d1("DB")?;

    let order = match request.order {
        Some(MessageOrder::Asc) => "ASC",
        Some(MessageOrder::Desc) => "DESC",
        None => "ASC",
    };

    let result = d1
        .prepare("SELECT * FROM message WHERE channel_id = ? AND timestamp >= ? AND timestamp <= ? ORDER BY timestamp ? LIMIT ?")
        .bind(&[
            request.channel_id.into(),
            request.from.into(),
            request.to.into(),
            order.into(),
            request.limit.into(),
        ])?
        .run()
        .await?;

    if let Some(message) = result.results::<Message>()?.first() {
        Response::from_json(message)
    } else {
        Response::error("No messages found", 404)
    }
}
