use serenity::{
    builder::{CreateCommand, CreateCommandOption},
    model::application::{CommandOptionType, ResolvedValue},
};

use crate::{commands::ManamiSlashCommand, Bot};
use serenity::model::application::ResolvedOption;

pub const SLASH_EFFORT_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "effort",
    usage: "/effort <effort>",
    description: "effortのレベルを変えるよ！",
    register,
    run: |option, ctx| {
        let opts = parse(option);
        Box::pin(async move { run_body(opts, ctx.bot).await })
    },
    is_local_command: true,
};

pub fn register() -> CreateCommand {
    let mut effort_option =
        CreateCommandOption::new(CommandOptionType::String, "effort", "努力レベル").required(false);
    // 選択肢は環境変数 LLM_EFFORT（カンマ区切り）から生成する（Discord の上限は 25）。
    for effort in ["minimal", "low", "medium", "high"].iter() {
        effort_option = effort_option.add_string_choice(effort.to_string(), effort.to_string());
    }

    CreateCommand::new("effort")
        .description("effortのレベルを変えるよ")
        .add_option(effort_option)
}

pub async fn run(option: Vec<ResolvedOption<'_>>, bot: &Bot) -> String {
    run_body(parse(option), bot).await
}

fn parse(option: Vec<ResolvedOption<'_>>) -> Option<String> {
    option
        .iter()
        .fold(None, |effort, option| match (option.name, &option.value) {
            ("effort", ResolvedValue::String(s)) => Some((*s).to_owned()),
            _ => effort,
        })
}

async fn run_body(effort: Option<String>, bot: &Bot) -> String {
    effort.map_or_else(
        || format!("今の努力レベルは{}だよ", bot.ai.get_effort()),
        |effort| {
            bot.ai.set_effort(effort.clone());
            format!("努力レベルを{effort}に変更したよ")
        },
    )
}
