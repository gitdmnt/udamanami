use serenity::{
    builder::{CreateCommand, CreateCommandOption},
    model::application::{CommandOptionType, ResolvedValue},
};

use crate::ai::GeminiModel;

use crate::{commands::StManamiSlashCommand, Bot};
use serenity::model::application::ResolvedOption;
pub const SLASH_GEMINI_COMMAND: StManamiSlashCommand = StManamiSlashCommand {
    name: "gemini",
    usage: "/gemini <model>",
    description: "Geminiの設定を変更するよ！",
    register,
    run: |option, bot| {
        let opts = parse(option, bot);
        Box::pin(async move { run_body(opts, bot).await })
    },
    is_local_command: true,
};

pub fn register() -> CreateCommand {
    CreateCommand::new("gemini")
        .description("Geminiの設定を変更するよ")
        .add_option(
            CreateCommandOption::new(CommandOptionType::String, "model", "モデル")
                .required(false)
                .add_string_choice("Gemini 2.0 Flash Lite", "gemini-2.0-flash-lite")
                .add_string_choice("Gemini 2.0 Flash", "gemini-2.0-flash")
                .add_string_choice("Gemini 2.5 Flash Preview", "gemini-2.5-flash-preview-04-17")
                .add_string_choice("Gemini 2.5 Pro Experimental", "gemini-2.5-pro-exp-03-25"),
        )
}

pub async fn run(option: Vec<ResolvedOption<'_>>, bot: &Bot) -> String {
    let model = option
        .iter()
        .fold(None, |model, option| match (option.name, &option.value) {
            ("model", ResolvedValue::String(s)) => Some(GeminiModel::from(*s)),
            _ => model,
        });

    if let Some(model) = model {
        let msg = format!("モデルを{}に変更したよ", model);
        bot.gemini.set_model(model);
        msg
    } else {
        match bot.gemini.generate().await {
            Ok(content) => content.replace("うだまなみ: ", ""),
            Err(e) => {
                format!("Error sending message: {:?}", e)
            }
        }
    }
}

fn parse(option: Vec<ResolvedOption<'_>>, _: &Bot) -> Option<GeminiModel> {
    option
        .iter()
        .fold(None, |model, option| match (option.name, &option.value) {
            ("model", ResolvedValue::String(s)) => Some(GeminiModel::from(*s)),
            _ => model,
        })
}

async fn run_body(model: Option<GeminiModel>, bot: &Bot) -> String {
    if let Some(model) = model {
        let msg = format!("モデルを{}に変更したよ", model);
        bot.gemini.set_model(model);
        msg
    } else {
        match bot.gemini.generate().await {
            Ok(content) => content.replace("うだまなみ: ", ""),
            Err(e) => {
                format!("Error sending message: {:?}", e)
            }
        }
    }
}
