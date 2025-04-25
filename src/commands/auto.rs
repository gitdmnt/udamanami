use crate::ai::GeminiModel;
use serenity::{
    builder::{CreateCommand, CreateCommandOption},
    model::application::{CommandOptionType, ResolvedValue},
};
use std::time::Duration;

use crate::{commands::StManamiSlashCommand, Bot};
use serenity::model::application::ResolvedOption;

pub struct SlashCommand;

const COMMAND_NAME: &str = "auto";

pub const SLASH_AUTO_COMMAND: StManamiSlashCommand = StManamiSlashCommand {
    name: COMMAND_NAME,
    description: "呼びかけられなくてもお返事するよ！",
    register,
    run: |option, bot| {
        let opts = parse(option, bot);
        Box::pin(async move { run_body(opts, bot).await })
    },
    is_local_command: true,
};

pub fn register() -> CreateCommand {
    CreateCommand::new(COMMAND_NAME)
        .description("[sec]秒以内の連続した会話に対して、[model]を使って必ず返信するよ")
        .add_option(
            CreateCommandOption::new(CommandOptionType::String, "model", "モデル")
                .required(false)
                .add_string_choice("Gemini 2.0 Flash Lite", "gemini-2.0-flash-lite")
                .add_string_choice("Gemini 2.0 Flash", "gemini-2.0-flash")
                .add_string_choice("Gemini 2.5 Flash Preview", "gemini-2.5-flash-preview-04-17")
                .add_string_choice("Gemini 2.5 Pro Experimental", "gemini-2.5-pro-exp-03-25"),
        )
        .add_option(
            CreateCommandOption::new(CommandOptionType::Integer, "sec", "秒数")
                .required(false)
                .max_int_value(0)
                .max_int_value(600),
        )
}

pub async fn run(option: Vec<ResolvedOption<'_>>, bot: &Bot) -> String {
    run_body(parse(option, bot), bot).await
}

fn parse(option: Vec<ResolvedOption<'_>>, bot: &Bot) -> (GeminiModel, Duration) {
    let model = option
        .iter()
        .fold(None, |model, option| match (option.name, &option.value) {
            ("model", ResolvedValue::String(s)) => Some(GeminiModel::from(*s)),
            _ => model,
        })
        .unwrap_or_else(|| bot.gemini.get_model());

    let sec = Duration::from_secs(
        option
            .iter()
            .fold(None, |sec, option| match (option.name, &option.value) {
                ("sec", ResolvedValue::Integer(s)) => Some(*s as u64),
                _ => sec,
            })
            .unwrap_or(300),
    );

    (model, sec)
}

async fn run_body((model, sec): (GeminiModel, Duration), bot: &Bot) -> String {
    bot.reply_to_all_mode
        .lock()
        .unwrap()
        .set(model.clone(), sec);

    let msg = format!(
        "（全レスモード：{}秒以内の連続した会話に対して、{}で返信するよ）",
        sec.as_secs(),
        model
    );
    msg
}
