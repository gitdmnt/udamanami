use serenity::{
    builder::{CreateCommand, CreateCommandOption},
    model::application::{CommandOptionType, ResolvedOption, ResolvedValue},
};

use crate::{ai::GeminiModel, Bot};

pub fn register() -> CreateCommand {
    CreateCommand::new("gemini")
        .description("Geminiの設定を変更するよ")
        .add_option(
            CreateCommandOption::new(CommandOptionType::String, "model", "モデル")
                .required(false)
                .add_string_choice("Gemini 2.0 Flash", "gemini-2.0-flash")
                .add_string_choice("Gemini 2.0 Flash Lite", "gemini-2.0-flash-lite"),
        )
}

pub async fn run(option: &[ResolvedOption<'_>], bot: &Bot) -> String {
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
