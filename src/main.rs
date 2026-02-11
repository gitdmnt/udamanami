use std::{env, str::FromStr};

use anyhow::Context as _;

use serenity::{
    model::id::{ChannelId, GuildId, RoleId},
    prelude::*,
};

use udamanami::ai;
use udamanami::db::BotDatabase;
use udamanami::Bot;

fn env_var(key: &str) -> Option<String> {
    env::var(key).ok()
}

fn env_var_required(key: &str) -> anyhow::Result<String> {
    env::var(key).with_context(|| format!("'{key}' was not found"))
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Get the discord token set in environment variables
    let token = env_var_required("DISCORD_TOKEN")?;

    // Set gateway intents, which decides what events the bot will be notified about
    let intents = GatewayIntents::GUILD_MESSAGES
        | GatewayIntents::MESSAGE_CONTENT
        | GatewayIntents::DIRECT_MESSAGES;

    let channel_ids_new = env_var("ROOMS_ID").map_or(vec![], |rooms| {
        rooms
            .split(',')
            .filter_map(|id| ChannelId::from_str(id.trim()).ok())
            .collect()
    });

    // ↓ここからのコードは将来的に取り除きたい
    let channel_ids_old: Vec<ChannelId> = vec![
        env_var("FREETALK1_ROOM_ID"),
        env_var("FREETALK2_ROOM_ID"),
        env_var("MADSISTERS_ROOM_ID"),
        env_var("SHYBOYS_ROOM_ID"),
        env_var("DEBUG_ROOM_ID"),
        env_var("HOSPITAL_ROOM_ID"),
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

    let debug_channel_id = env_var("DEBUG_ROOM_ID")
        .map(|id| ChannelId::from_str(&id).unwrap())
        .unwrap_or_default();

    let disabled_commands = env_var("DISABLED_COMMANDS")
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

    let guild_id = env_var("DISCORD_GUILD_ID")
        .map(|id| GuildId::from_str(&id).unwrap())
        .unwrap();

    let jail_mark_role_id = env_var("JAIL_MARK_ROLE_ID")
        .map(|id| RoleId::from_str(&id).unwrap())
        .unwrap();

    let jail_main_role_id = env_var("JAIL_MAIN_ROLE_ID")
        .map(|id| RoleId::from_str(&id).unwrap())
        .unwrap();

    let commit_hash = env_var("COMMIT_HASH");

    let commit_date = env_var("COMMIT_DATE");

    let gemini = ai::GeminiAI::manami(&env_var_required("GEMINI_API_KEY")?);

    let database_path = env_var("DATABASE_PATH").unwrap_or_else(|| "./db.sqlite".to_owned());
    let database = BotDatabase::new(&database_path).await?;

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

    let mut client = Client::builder(&token, intents)
        .event_handler(bot)
        .await
        .expect("Err creating client");

    client.start().await?;

    Ok(())
}
