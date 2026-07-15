//! ユーザーとそれに関連する情報のCRUD

use serde::{Deserialize, Serialize};
use worker::*;

use crate::channel::ChannelId;

pub type UserId = String;

#[derive(Debug, Deserialize, Serialize)]
struct User {
    user_id: UserId,
    username: String,
    room_pointer: Option<ChannelId>,
}

pub async fn upsert_user(mut req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let body = req.text().await?;
    let Ok(user) = serde_json::from_str::<User>(&body) else {
        return Response::error("Failed to parse request body", 400);
    };

    let d1 = ctx.env.d1("DB")?;

    let _ = d1
        .prepare(
            "INSERT INTO user (user_id, username) VALUES (?, ?)
ON CONFLICT (user_id) DO UPDATE SET username = excluded.username;
",
        )
        .bind(&[
            user.user_id.into(),
            user.username.into(),
            user.room_pointer.into(),
        ])?
        .run()
        .await?;

    Response::ok("User upserted successfully")
}

pub async fn set_room_pointer(mut req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let body = req.text().await?;
    let Ok(user) = serde_json::from_str::<User>(&body) else {
        return Response::error("Failed to parse request body", 400);
    };

    let d1 = ctx.env.d1("DB")?;

    let _ = d1
        .prepare("UPDATE user SET room_pointer = ? WHERE user_id = ?")
        .bind(&[user.room_pointer.into(), user.user_id.into()])?
        .run()
        .await?;

    Response::ok("Room pointer set successfully")
}

pub async fn get_room_pointer(mut req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let body = req.text().await?;
    let Ok(user) = serde_json::from_str::<User>(&body) else {
        return Response::error("Failed to parse request body", 400);
    };

    let d1 = ctx.env.d1("DB")?;

    let _ = d1
        .prepare("SELECT room_pointer FROM user WHERE user_id = ?")
        .bind(&[user.user_id.into()])?
        .run()
        .await?;

    Response::ok("Room pointer set successfully")
}
