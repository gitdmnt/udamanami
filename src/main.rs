use std::{convert::Into, str::FromStr};

use anyhow::Context as _;

use serenity::{
    model::id::{ChannelId, GuildId, RoleId},
    prelude::*,
};
use shuttle_runtime::SecretStore;

use udamanami::ai;
use udamanami::db::BotDatabase;
use udamanami::Bot;

#[shuttle_runtime::main]
async fn serenity(
    #[shuttle_runtime::Secrets] secrets: SecretStore,
) -> shuttle_serenity::ShuttleSerenity {
    // Get the discord token set in `Secrets.toml`
    let token = secrets
        .get("DISCORD_TOKEN")
        .context("'DISCORD_TOKEN' was not found")?;

    // Set gateway intents, which decides what events the bot will be notified about
    let intents = GatewayIntents::GUILD_MESSAGES
        | GatewayIntents::MESSAGE_CONTENT
        | GatewayIntents::DIRECT_MESSAGES;

    let channel_ids_new = secrets.get("ROOMS_ID").map_or(vec![], |rooms| {
        rooms
            .split(',')
            .filter_map(|id| ChannelId::from_str(id.trim()).ok())
            .collect()
    });

    // ↓ここからのコードは将来的に取り除きたい
    let channel_ids_old: Vec<ChannelId> = vec![
        secrets.get("FREETALK1_ROOM_ID"),
        secrets.get("FREETALK2_ROOM_ID"),
        secrets.get("MADSISTERS_ROOM_ID"),
        secrets.get("SHYBOYS_ROOM_ID"),
        secrets.get("DEBUG_ROOM_ID"),
        secrets.get("HOSPITAL_ROOM_ID"),
    ]
    .into_iter()
    .filter_map(|id| id.and_then(|id| ChannelId::from_str(&id).ok()))
    .collect();

    let channel_ids = if channel_ids_old.len() > 1 {
        channel_ids_old
    } else {
        channel_ids_new
    };
    // ↑ここまで

    let debug_channel_id = secrets
        .get("DEBUG_ROOM_ID")
        .map(|id| ChannelId::from_str(&id).unwrap())
        .unwrap_or_default();

    let disabled_commands = secrets
        .get("DISABLED_COMMANDS")
        .map(|commands| {
            commands
                .split(',')
                .map(|command| command.trim().to_owned())
                .collect::<Vec<String>>()
        })
        .unwrap_or_default();
    let disabled_commands = disabled_commands
        .iter()
        .map(|s| s.as_str())
        .collect::<Vec<_>>();

    let guild_id = secrets
        .get("DISCORD_GUILD_ID")
        .map(|id| GuildId::from_str(&id).unwrap())
        .unwrap();

    let jail_mark_role_id = secrets
        .get("JAIL_MARK_ROLE_ID")
        .map(|id| RoleId::from_str(&id).unwrap())
        .unwrap();

    let jail_main_role_id = secrets
        .get("JAIL_MAIN_ROLE_ID")
        .map(|id| RoleId::from_str(&id).unwrap())
        .unwrap();

    let commit_hash = secrets.get("COMMIT_HASH");

    let commit_date = secrets.get("COMMIT_DATE");

    let gemini = ai::GeminiAI::manami(&secrets.get("GEMINI_API_KEY").unwrap());

    let database = BotDatabase::new("./db.sqlite").await?;

    let bot = Bot::new(
        channel_ids,
        debug_channel_id,
        guild_id,
        jail_mark_role_id,
        jail_main_role_id,
        gemini,
        commit_hash,
        commit_date,
        &disabled_commands,
        database,
    )
    .await;

    let client = Client::builder(&token, intents)
        .event_handler(bot)
        .await
        .expect("Err creating client");

    Ok(client.into())
}
