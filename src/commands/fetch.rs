use serenity::all::{GetMessages, UserId};
use std::{cmp::min, collections::HashMap};

use crate::commands::ManamiPrefixCommand;

use super::CommandContext;
pub const PREFIX_FETCH_COMMAND: ManamiPrefixCommand = ManamiPrefixCommand {
    name: "fetch",
    alias: &[],
    usage: "!fetch [count]",
    description: "このチャンネルに投稿された、覚えているのより古いメッセージを取得するよ！",
    run: |ctx| Box::pin(run(ctx)),
    is_dm_command: false,
    is_guild_command: true,
};

const FETCH_DEFAULT: u64 = 100;
const FETCH_MAX: u64 = 1000;

pub async fn run(ctx: CommandContext<'_>) {
    let count = ctx
        .args()
        .first()
        .and_then(|arg| arg.parse::<u64>().ok())
        .unwrap_or(FETCH_DEFAULT)
        .clamp(0, FETCH_MAX);

    let mut oldest = ctx
        .bot
        .database
        .fetch_oldest_message(&ctx.channel_id)
        .await
        .ok()
        .flatten()
        .map(|m| m.message_id);

    let oldest_start = oldest;

    let mut remaining = count;
    let mut log = Vec::new();
    while remaining > 0 {
        let mut messages = ctx
            .channel_id
            .messages(
                ctx.cache_http(),
                oldest.map_or_else(
                    || GetMessages::new().limit(min(remaining, 100) as u8),
                    |oldest| {
                        GetMessages::new()
                            .limit(min(remaining, 100) as u8)
                            .before(oldest)
                    },
                ),
            )
            .await
            .unwrap_or_default();

        let count = messages.len();
        oldest = messages.last().map(|m| m.id);

        log.append(&mut messages);

        if count == 0 {
            break;
        }
        remaining -= count as u64;
    }

    let default_channel_name = "".to_owned();
    let channel_name = ctx
        .channel_id
        .name(&ctx.cache_http())
        .await
        .unwrap_or(default_channel_name);

    let mut user_info: HashMap<UserId, String> = HashMap::new();
    for message in &log {
        if user_info.contains_key(&message.author.id) {
            continue;
        }

        let user_name = message.author_nick(&ctx.cache_http()).await;
        let user_name = user_name
            .as_deref()
            .unwrap_or_else(|| message.author.display_name());

        user_info.insert(message.author.id, user_name.to_owned());
    }

    let _ = ctx
        .bot
        .database
        .insert_many_guild_messages(&log, user_info, (ctx.channel_id, channel_name))
        .await;

    let oldest = ctx
        .bot
        .database
        .fetch_oldest_message(&ctx.channel_id)
        .await
        .ok()
        .flatten()
        .map(|m| m.message_id);

    if let Some(oldest) = oldest {
        if let Some(oldest_start) = oldest_start {
            ctx.channel_id
                .say(
                    ctx.cache_http(),
                    format!(
                        "メッセージ読み込み: {} → {}",
                        oldest.link(ctx.channel_id, ctx.guild_id),
                        oldest_start.link(ctx.channel_id, ctx.guild_id)
                    ),
                )
                .await
                .ok();
        } else {
            ctx.channel_id
                .say(
                    ctx.cache_http(),
                    format!(
                        "メッセージ読み込み: {}",
                        oldest.link(ctx.channel_id, ctx.guild_id)
                    ),
                )
                .await
                .ok();
        }
    }
}
