//! チャンネルのCRUD

use serde::Deserialize;
use udamanami_shared::{Channel, ChannelReplySetting, SetChannelSummarized, SummarizeCandidate};
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
    let Some(channel_id) = crate::query_param(&url, "channel_id") else {
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

/// 会話セッションの自動要約が対象にすべきチャンネルを返す
/// 未要約メッセージを持つチャンネルのうち、idle_before 時間無言が続いたか、未要約が min_pending 件たまったものだけが返る。
pub async fn summarize_candidates(req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let url = req.url()?;

    let Some(idle_before) = crate::query_param(&url, "idle_before") else {
        return Response::error("Missing query parameter: idle_before", 400);
    };
    let Some(min_pending) = crate::query_param(&url, "min_pending").and_then(|v| v.parse::<u32>().ok())
    else {
        return Response::error("Missing or invalid query parameter: min_pending", 400);
    };

    let d1 = ctx.env.d1("DB")?;

    // min_pending は f64 でバインドする。D1 は BigInt を受け付けない。
    let res = d1
        .prepare(
            "SELECT c.channel_id, c.name, c.last_summarized_at,
                    MIN(m.timestamp) AS first_pending_at,
                    MAX(m.timestamp) AS last_message_at,
                    COUNT(*)         AS pending_count
             FROM channel c
             JOIN message m
               ON m.channel_id = c.channel_id
              AND (c.last_summarized_at IS NULL OR m.timestamp > c.last_summarized_at)
             GROUP BY c.channel_id
             HAVING MAX(m.timestamp) <= ?1 OR COUNT(*) >= ?2
             ORDER BY MIN(m.timestamp) ASC",
        )
        .bind(&[idle_before.into(), f64::from(min_pending).into()])?
        .run()
        .await?;

    Response::from_json(&res.results::<SummarizeCandidate>()?)
}

/// 自動要約の進捗を前進させる。
pub async fn set_summarized(mut req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let body = req.text().await?;
    let Ok(progress) = serde_json::from_str::<SetChannelSummarized>(&body) else {
        return Response::error("Failed to parse request body", 400);
    };

    let d1 = ctx.env.d1("DB")?;

    // MAX() で last_summarized_at を単調増加させ、進捗の巻き戻りを防ぐ。COALESCE は NULL の既存行対策。
    let _ = d1
        .prepare(
            "INSERT INTO channel (channel_id, is_thread, name, last_summarized_at)
            VALUES (?, 0, '', ?)
            ON CONFLICT(channel_id) DO UPDATE SET
                last_summarized_at =
                    MAX(COALESCE(last_summarized_at, ''), excluded.last_summarized_at)",
        )
        .bind(&[
            progress.channel_id.into(),
            progress.last_summarized_at.into(),
        ])?
        .run()
        .await?;

    Response::ok("Channel summary progress updated successfully")
}
