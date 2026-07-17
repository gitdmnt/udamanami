//! チャンネルのCRUD

use serde::Deserialize;
use udamanami_shared::{Channel, ChannelReplySetting};
use worker::*;

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

/// 自発反応の設定を部分更新する(PUT /channel/reply)。
/// `None` のフィールドは COALESCE で据え置き、行が無ければ name/is_thread を
/// プレースホルダで作る(既存行は ON CONFLICT で name/is_thread を上書きしない)。
pub async fn set_reply_setting(mut req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let body = req.text().await?;
    let Ok(setting) = serde_json::from_str::<ChannelReplySetting>(&body) else {
        return Response::error("Failed to parse request body", 400);
    };

    let d1 = ctx.env.d1("DB")?;

    let _ = d1
        .prepare(
            "INSERT INTO channel (channel_id, is_thread, name, reply_enabled, reply_rate)
            VALUES (?, 0, '', ?, ?)
            ON CONFLICT(channel_id) DO UPDATE SET
                reply_enabled = COALESCE(excluded.reply_enabled, reply_enabled),
                reply_rate    = COALESCE(excluded.reply_rate, reply_rate)",
        )
        .bind(&[
            setting.channel_id.into(),
            crate::opt_to_js(setting.reply_enabled),
            crate::opt_to_js(setting.reply_rate),
        ])?
        .run()
        .await?;

    Response::ok("Channel reply setting updated successfully")
}

/// D1 は BOOLEAN/INTEGER を数値で返すため、reply_enabled は i64 で受けて変換する。
#[derive(Deserialize)]
struct ReplySettingRow {
    channel_id: String,
    reply_enabled: Option<i64>,
    reply_rate: Option<u32>,
}

/// 自発反応の設定を取得する(GET /channel/reply?channel_id=)。行が無ければ null。
pub async fn get_reply_setting(req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let url = req.url()?;
    let Some(channel_id) = url
        .query_pairs()
        .find(|(k, _)| k == "channel_id")
        .map(|(_, v)| v.into_owned())
    else {
        return Response::error("Missing query parameter: channel_id", 400);
    };

    let d1 = ctx.env.d1("DB")?;

    let res = d1
        .prepare("SELECT channel_id, reply_enabled, reply_rate FROM channel WHERE channel_id = ?")
        .bind(&[channel_id.into()])?
        .run()
        .await?;

    let setting = res
        .results::<ReplySettingRow>()?
        .into_iter()
        .next()
        .map(|row| ChannelReplySetting {
            channel_id: row.channel_id,
            reply_enabled: row.reply_enabled.map(|v| v != 0),
            reply_rate: row.reply_rate,
        });

    Response::from_json(&setting)
}
