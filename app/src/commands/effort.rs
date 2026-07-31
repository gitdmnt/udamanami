use serenity::{
    builder::{CreateCommand, CreateCommandOption},
    model::application::{CommandOptionType, ResolvedValue},
};

use crate::ai::Stage;
use crate::{commands::ManamiSlashCommand, Bot};
use serenity::model::application::ResolvedOption;

use super::stage_option;

pub const SLASH_EFFORT_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "effort",
    usage: "/effort <stage> [effort]",
    description: "reasoning effortを変えるよ！",
    register,
    run: |option, ctx| {
        let opts = parse(option);
        Box::pin(async move { run_body(opts, ctx.bot).await })
    },
    is_local_command: true,
};

pub fn register() -> CreateCommand {
    let mut effort_option = CreateCommandOption::new(
        CommandOptionType::String,
        "effort",
        "reasoning effortを変えるよ",
    )
    .required(false);
    for effort in ["none", "low", "medium", "high"] {
        effort_option = effort_option.add_string_choice(effort, effort);
    }

    CreateCommand::new("effort")
        .description("reasoning effortを変えるよ")
        .add_option(stage_option("どのフェーズのreasoning effortを変えるか"))
        .add_option(effort_option)
}

pub async fn run(option: Vec<ResolvedOption<'_>>, bot: &Bot) -> String {
    run_body(parse(option), bot).await
}

fn parse(option: Vec<ResolvedOption<'_>>) -> (Option<Stage>, Option<String>) {
    option.iter().fold((None, None), |(stage, effort), option| {
        match (option.name, &option.value) {
            ("stage", ResolvedValue::String(s)) => (Stage::parse(s), effort),
            ("effort", ResolvedValue::String(s)) => (stage, Some((*s).to_owned())),
            _ => (stage, effort),
        }
    })
}

async fn run_body((stage, effort): (Option<Stage>, Option<String>), bot: &Bot) -> String {
    let Some(stage) = stage else {
        return "planner と performer, どっちのreasoning effortを変えるか教えて！".to_owned();
    };

    effort.map_or_else(
        || {
            format!(
                "今の{}のreasoning effortは{}だよ",
                stage.label(),
                bot.ai.get_effort(stage)
            )
        },
        |effort| {
            bot.ai.set_effort(stage, effort.clone());
            format!("{}のreasoning effortを{effort}に変更したよ", stage.label())
        },
    )
}
