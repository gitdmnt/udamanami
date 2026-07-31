use serenity::{
    builder::{CreateCommand, CreateCommandOption},
    model::application::{CommandOptionType, ResolvedValue},
};

use crate::ai::{available_models, Stage};

use crate::{commands::ManamiSlashCommand, Bot};
use serenity::model::application::ResolvedOption;

use super::stage_option;

pub const SLASH_MODEL_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "model",
    usage: "/model <stage> [model]",
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
        .add_option(stage_option("どのフェーズのモデルを変えるか"))
        .add_option(model_option)
}

pub async fn run(option: Vec<ResolvedOption<'_>>, bot: &Bot) -> String {
    run_body(parse(option), bot).await
}

fn parse(option: Vec<ResolvedOption<'_>>) -> (Option<Stage>, Option<String>) {
    option.iter().fold((None, None), |(stage, model), option| {
        match (option.name, &option.value) {
            ("stage", ResolvedValue::String(s)) => (Stage::parse(s), model),
            ("model", ResolvedValue::String(s)) => (stage, Some((*s).to_owned())),
            _ => (stage, model),
        }
    })
}

async fn run_body((stage, model): (Option<Stage>, Option<String>), bot: &Bot) -> String {
    let Some(stage) = stage else {
        return "planner と performer, どっちのモデルを変えるか教えて！".to_owned();
    };

    model.map_or_else(
        || {
            format!(
                "今の{}のモデルは{}だよ",
                stage.label(),
                bot.ai.get_model(stage)
            )
        },
        |model| {
            bot.ai.set_model(stage, model.clone());
            format!("{}のモデルを{model}に変更したよ", stage.label())
        },
    )
}
