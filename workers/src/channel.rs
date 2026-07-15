//! チャンネルのCRUD

use serde::{Deserialize, Serialize};
use worker::*;

pub type ChannelId = String;

#[derive(Debug, Deserialize, Serialize)]
struct Channel {
    channel_id: ChannelId,
    is_thread: bool,
    name: String,
}

pub async fn upsert_channel(mut req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let body = req.text().await?;
    let Ok(channel) = serde_json::from_str::<Channel>(&body) else {
        return Response::error("Failed to parse request body", 400);
    };

    let d1 = ctx.env.d1("DB")?;

    let _ = d1
        .prepare(
            "INSERT INTO channel (channel_id, is_thread, name) VALUES (?, ?, ?)
ON CONFLICT (channel_id) DO UPDATE SET is_thread = excluded.is_thread, name = excluded.name;
",
        )
        .bind(&[
            channel.channel_id.into(),
            channel.is_thread.into(),
            channel.name.into(),
        ])?
        .run()
        .await?;

    Response::ok("Channel upserted successfully")
}
