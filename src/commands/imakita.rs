use crate::ManamiPrefixCommand;

use chrono::TimeDelta;

use super::CommandContext;

const COMMAND_NAME: &str = "imakita";

pub const PREFIX_IMAKITA_COMMAND: ManamiPrefixCommand = ManamiPrefixCommand {
    name: COMMAND_NAME,
    usage: "!imakita [minutes]",
    alias: &["今北産業"],
    description: "今北産業",
    run: |ctx| {
        let time = ctx.args().first().and_then(|arg| arg.parse::<u32>().ok());
        Box::pin(async move { run_body(time, &ctx).await })
    },
    is_dm_command: false,
    is_guild_command: true,
};

/*
pub fn register() -> serenity::builder::CreateCommand {
    serenity::builder::CreateCommand::new("imakita")
        .description("今北産業")
        .add_option(
            serenity::builder::CreateCommandOption::new(
                serenity::model::application::CommandOptionType::Integer,
                "time",
                "何分間のログを取得するか",
            )
            .required(false),
        )
}

pub fn parse_options(options: Vec<serenity::model::application::ResolvedOption>) -> Option<u32> {
    options
        .iter()
        .fold(None, |time, option| match (option.name, &option.value) {
            ("time", serenity::model::application::ResolvedValue::Integer(i)) => Some(*i as u32),
            _ => time,
        })
}
*/

pub async fn run_body(time: Option<u32>, ctx: &CommandContext<'_>) {
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

    let gemini_contents = fetch_result
        .into_iter()
        .map(|m| m.gemini_content(self_id))
        .collect::<Vec<_>>();

    let result = ctx
        .bot
        .gemini
        .generate_matome(gemini_contents)
        .await
        .unwrap_or_else(|e| {
            println!("Failed to generate matome: {e}");
            "".to_owned()
        });

    ctx.channel_id.say(ctx.cache_http(), result).await.ok();
}
//
