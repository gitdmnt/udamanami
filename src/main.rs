use std::{
    convert::Into,
    str::FromStr,
    sync::{Arc, Mutex},
};

use anyhow::Context as _;
use dashmap::DashMap;

use serenity::{
    model::id::{ChannelId, GuildId, RoleId},
    prelude::*,
};
use shuttle_runtime::SecretStore;

use udamanami::ai;
use udamanami::Bot;
use udamanami::{prefix_commands, slash_commands};

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

    let userdata = DashMap::new();
    let channel_ids = secrets.get("ROOMS_ID").map_or_else(
        || {
            vec![
                secrets.get("FREETALK1_ROOM_ID"),
                secrets.get("FREETALK2_ROOM_ID"),
                secrets.get("MADSISTERS_ROOM_ID"),
                secrets.get("SHYBOYS_ROOM_ID"),
                secrets.get("DEBUG_ROOM_ID"),
            ]
            .into_iter()
            .map(|id| ChannelId::from_str(&id.unwrap()).unwrap())
            .collect()
        },
        |rooms| {
            rooms
                .split(',')
                .map(|id| ChannelId::from_str(id).unwrap())
                .collect()
        },
    );

    let debug_channel_id = secrets
        .get("DEBUG_ROOM_ID")
        .map(|id| ChannelId::from_str(&id).unwrap())
        .unwrap_or_default();

    let disabled_commands = secrets
        .get("DISABLED_COMMANDS")
        .map(|commands| {
            commands
                .split(',')
                .map(|command| command.to_owned())
                .collect::<Vec<String>>()
        })
        .unwrap_or_default();
    let disabled_commands = disabled_commands
        .iter()
        .map(|s| s.as_str())
        .collect::<Vec<_>>();

    // 取得できなければ KOCHIKITE_GUILD_ID を使う
    let guild_id = secrets
        .get("DISCORD_GUILD_ID")
        .map(|id| GuildId::from_str(&id).unwrap())
        .unwrap();

    let erogaki_role_id = secrets
        .get("EROGAKI_ROLE_ID")
        .map(|id| RoleId::from_str(&id).unwrap())
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

    let variables = DashMap::new();

    let gemini = ai::GeminiAI::new(&secrets.get("GEMINI_API_KEY").unwrap());

    let jail_process = Arc::new(DashMap::new());

    let jail_id = Arc::new(Mutex::new(0));

    let reply_to_all_mode = Arc::new(Mutex::new(udamanami::ReplyToAllModeData::blank()));

    let prefix_commands = prefix_commands(&disabled_commands);
    let slash_commands = slash_commands(&disabled_commands);

    let client = Client::builder(&token, intents)
        .event_handler(Bot {
            userdata,
            jail_process,
            jail_id,
            channel_ids,
            debug_channel_id,
            guild_id,
            erogaki_role_id,
            jail_mark_role_id,
            jail_main_role_id,
            commit_hash,
            commit_date,
            variables,
            reply_to_all_mode,
            gemini,
            prefix_commands,
            slash_commands,
        })
        .await
        .expect("Err creating client");

    Ok(client.into())
}
