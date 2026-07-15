use crate::ManamiPrefixCommand;

use chrono::TimeDelta;
use serenity::model::application::{ResolvedOption, ResolvedValue};

use super::{CommandContext, ManamiSlashCommand};

const COMMAND_NAME: &str = "imakita";

pub const PREFIX_IMAKITA_COMMAND: ManamiPrefixCommand = ManamiPrefixCommand {
    name: COMMAND_NAME,
    usage: "!imakita [minutes]",
    alias: &["今北産業"],
    description: "今北産業",
    run: |ctx| {
        let time = ctx.args().first().and_then(|arg| arg.parse::<u32>().ok());
        Box::pin(async move {
            let result = run_body(time, &ctx).await;
            ctx.channel_id.say(ctx.cache_http(), result).await.ok();
        })
    },
    is_dm_command: false,
    is_guild_command: true,
};

pub const SLASH_IMAKITA_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: COMMAND_NAME,
    usage: "/imakita [minutes]",
    description: "今北産業",
    register,
    run: |options, ctx| {
        let time = parse_options(options);
        Box::pin(async move {
            let result = run_body(time, &ctx).await;
            if result.is_empty() {
                "うまくまとめられなかったよ".to_owned()
            } else {
                result
            }
        })
    },
    is_local_command: true,
};

pub fn register() -> serenity::builder::CreateCommand {
    serenity::builder::CreateCommand::new(COMMAND_NAME)
        .description("今北産業")
        .add_option(
            serenity::builder::CreateCommandOption::new(
                serenity::model::application::CommandOptionType::Integer,
                "minutes",
                "何分間のログを取得するか",
            )
            .required(false)
            .min_int_value(0),
        )
}

fn parse_options(options: Vec<ResolvedOption<'_>>) -> Option<u32> {
    options
        .iter()
        .fold(None, |time, option| match (option.name, &option.value) {
            ("minutes", ResolvedValue::Integer(i)) => Some(*i as u32),
            _ => time,
        })
}

pub async fn run_body(time: Option<u32>, ctx: &CommandContext<'_>) -> String {
    let channel_id = ctx.channel_id;
    let self_id = &ctx.ctx.http.get_current_user().await.unwrap().id;

    let fetch_result = match time {
        Some(time) => ctx
            .bot
            .database
            .fetch_log_by_duration(&channel_id, TimeDelta::minutes(time as i64))
            .await
            .unwrap(),
        None => ctx
            .bot
            .database
            .fetch_log_until_gap(&channel_id, TimeDelta::minutes(60))
            .await
            .unwrap(),
    };

    let chat_messages = fetch_result
        .into_iter()
        .map(|m| m.to_chat_message(self_id))
        .collect::<Vec<_>>();

    ctx.bot
        .ai
        .generate_matome(chat_messages)
        .await
        .unwrap_or_else(|e| {
            println!("Failed to generate matome: {e}");
            String::new()
        })
}
