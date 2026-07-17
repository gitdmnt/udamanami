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

    // 代筆先を /channel で設定していないユーザーの既定の送信先。
    let default_channel_id = env_var_required("DEFAULT_CHANNEL_ID").and_then(|id| {
        ChannelId::from_str(id.trim()).context("'DEFAULT_CHANNEL_ID' is not a valid channel ID")
    })?;

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
        .unwrap_or_default();

    let jail_main_role_id = env_var("JAIL_MAIN_ROLE_ID")
        .map(|id| RoleId::from_str(&id).unwrap())
        .unwrap_or_default();

    let commit_hash = env_var("COMMIT_HASH");

    let commit_date = env_var("COMMIT_DATE");

    // LLM 設定。OpenAI 互換エンドポイントを通すので、base_url / api_key / model を
    // 変えるだけで各種プロバイダを使える。利用可能モデルは LLM_MODELS（カンマ区切り）。
    let llm_base_url =
        env_var("LLM_BASE_URL").unwrap_or_else(|| "https://api.openai.com/v1".to_owned());
    let llm_api_key = env_var("LLM_API_KEY")
        .or_else(|| env_var("OPENAI_API_KEY"))
        .context("'LLM_API_KEY' (or 'OPENAI_API_KEY') was not found")?;
    let llm_model = env_var("LLM_MODEL")
        .or_else(|| ai::available_models().into_iter().next())
        .unwrap_or_else(|| "gpt-5.4-nano".to_owned());
    let llm_effort = env_var("LLM_EFFORT").unwrap_or_else(|| "minimal".to_owned());
    let ai = ai::ManamiAi::manami(&llm_base_url, &llm_api_key, &llm_model, &llm_effort)?;

    let workers_api_url = env_var("WORKERS_API_URL").context("'WORKERS_API_URL' was not found")?;
    let workers_api_token =
        env_var("WORKERS_API_TOKEN").context("'WORKERS_API_TOKEN' was not found")?;
    let database = BotDatabase::new(workers_api_url, workers_api_token);

    let bot = Bot::new(
        default_channel_id,
        debug_channel_id,
        guild_id,
        jail_mark_role_id,
        jail_main_role_id,
        ai,
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
