use crate::ai::GeminiModel;
use serenity::{
    builder::{CreateCommand, CreateCommandOption},
    model::application::{CommandOptionType, ResolvedValue},
};
use std::time::Duration;

use crate::{commands::ManamiSlashCommand, db::BotDatabase, Bot};
use serenity::model::application::ResolvedOption;

const COMMAND_NAME: &str = "fetch";

pub const SLASH_FETCH_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: COMMAND_NAME,
    usage: "/fetch [count]",
    description: "チャンネルのログを読み込むよ！",
    register,
    run: |option, bot| {
        let opts = parse_options(option, bot);
        Box::pin(async move { run_body(opts, bot).await })
    },
    is_local_command: true,
};

pub fn register() -> CreateCommand {
    CreateCommand::new(COMMAND_NAME)
        .description("[sec]秒以内の連続した会話に対して、[model]を使って必ず返信するよ")
        .add_option(
            CreateCommandOption::new(CommandOptionType::Integer, "count", "取得件数")
                .required(false)
                .min_int_value(0)
                .max_int_value(1000),
        )
}

pub async fn run(option: Vec<ResolvedOption<'_>>, bot: &Bot) -> String {
    run_body(parse_options(option, bot), bot).await
}

fn parse_options(option: Vec<ResolvedOption<'_>>, bot: &Bot) -> u32 {
    let count = option
        .iter()
        .fold(None, |count, option| match (option.name, &option.value) {
            ("count", ResolvedValue::Integer(i)) => Some(*i as u32),
            _ => count,
        })
        .unwrap_or(100);

    count
}

async fn run_body(count: u32, bot: &Bot) -> String {
    let mut response = String::new();

    let oldest = bot.database.fetch_oldest_message().await;

    match oldest {
        Some(oldest) => {
            let messages = bot
                .channel_id
                .messages(&bot.http, |m| m.before(oldest).limit(count))
                .await
                .unwrap();

            for message in messages {
                response.push_str(&format!("{}: {}\n", message.author.name, message.content));
            }
        }
        None => {
            response.push_str("No messages found.");
        }
    }
}
