use serenity::{
    builder::{CreateCommand, CreateCommandOption},
    model::application::{CommandOptionType, ResolvedValue},
};

use crate::ai::GeminiModel;

use crate::{commands::ManamiSlashCommand, Bot};
use serenity::model::application::ResolvedOption;
pub const SLASH_GEMINI_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "gemini",
    usage: "/gemini <model>",
    description: "Geminiの設定を変更するよ！",
    register,
    run: |option, ctx| {
        let opts = parse(option, ctx.bot);
        Box::pin(async move { run_body(opts, ctx.bot).await })
    },
    is_local_command: true,
};

pub fn register() -> CreateCommand {
    CreateCommand::new("gemini")
        .description("Geminiの設定を変更するよ")
        .add_option(
            CreateCommandOption::new(CommandOptionType::String, "model", "モデル")
                .required(false)
                .add_string_choice("Gemini 2.5 Flash Lite", "gemini-2.5-flash-lite")
                .add_string_choice("Gemini 2.5 Flash", "gemini-2.5-flash")
                .add_string_choice("Gemini 2.5 Pro", "gemini-2.5-pro"),
        )
}

pub async fn run(option: Vec<ResolvedOption<'_>>, bot: &Bot) -> String {
    run_body(parse(option, bot), bot).await
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
    model.map_or_else(|| {
        let current_model = bot.gemini.get_model();
        let msg = format!("モデルを{current_model}に変更したよ");
        msg
    }, |model| {
        let msg = format!("モデルを{model}に変更したよ");
        bot.gemini.set_model(model);
        msg
    })
}
