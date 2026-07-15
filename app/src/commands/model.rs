use serenity::{
    builder::{CreateCommand, CreateCommandOption},
    model::application::{CommandOptionType, ResolvedValue},
};

use crate::ai::available_models;

use crate::{commands::ManamiSlashCommand, Bot};
use serenity::model::application::ResolvedOption;

pub const SLASH_MODEL_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "model",
    usage: "/model <model>",
    description: "使うモデルを変えるよ！",
    register,
    run: |option, ctx| {
        let opts = parse(option);
        Box::pin(async move { run_body(opts, ctx.bot).await })
    },
    is_local_command: true,
};

pub fn register() -> CreateCommand {
    let mut model_option =
        CreateCommandOption::new(CommandOptionType::String, "model", "モデル").required(false);
    // 選択肢は環境変数 LLM_MODELS（カンマ区切り）から生成する（Discord の上限は 25）。
    for model in available_models().into_iter().take(25) {
        model_option = model_option.add_string_choice(model.clone(), model);
    }

    CreateCommand::new("model")
        .description("使うモデルを変えるよ")
        .add_option(model_option)
}

pub async fn run(option: Vec<ResolvedOption<'_>>, bot: &Bot) -> String {
    run_body(parse(option), bot).await
}

fn parse(option: Vec<ResolvedOption<'_>>) -> Option<String> {
    option
        .iter()
        .fold(None, |model, option| match (option.name, &option.value) {
            ("model", ResolvedValue::String(s)) => Some((*s).to_owned()),
            _ => model,
        })
}

async fn run_body(model: Option<String>, bot: &Bot) -> String {
    model.map_or_else(
        || format!("今のモデルは{}だよ", bot.ai.get_model()),
        |model| {
            bot.ai.set_model(model.clone());
            format!("モデルを{model}に変更したよ")
        },
    )
}
