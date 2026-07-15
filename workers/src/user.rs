//! ユーザーとそれに関連する情報のCRUD

use udamanami_shared::User;
use worker::*;

pub async fn upsert_user(mut req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let body = req.text().await?;
    let Ok(user) = serde_json::from_str::<User>(&body) else {
        return Response::error("Failed to parse request body", 400);
    };

    let d1 = ctx.env.d1("DB")?;

    let _ = d1
        .prepare(
            "INSERT INTO user (user_id, username, room_pointer) VALUES (?, ?, ?)
ON CONFLICT (user_id) DO UPDATE SET username = excluded.username, room_pointer = excluded.room_pointer;
",
        )
        .bind(&[
            user.user_id.into(),
            user.username.into(),
            crate::opt_to_js(user.room_pointer),
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
        .bind(&[crate::opt_to_js(user.room_pointer), user.user_id.into()])?
        .run()
        .await?;

    Response::ok("Room pointer set successfully")
}

/// クエリパラメータ: user_id (必須)
pub async fn get_room_pointer(req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let url = req.url()?;
    let Some(user_id) = url
        .query_pairs()
        .find(|(k, _)| k == "user_id")
        .map(|(_, v)| v.into_owned())
    else {
        return Response::error("Missing query parameter: user_id", 400);
    };

    let d1 = ctx.env.d1("DB")?;

    let res = d1
        .prepare("SELECT user_id, username, room_pointer FROM user WHERE user_id = ?")
        .bind(&[user_id.into()])?
        .run()
        .await?;

    Response::from_json(&res.results::<User>()?.first().and_then(|u| u.room_pointer.clone()))
}
