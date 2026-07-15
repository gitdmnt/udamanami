use serenity::all::{CommandOptionType, ResolvedOption, ResolvedValue};

use crate::commands::var::{eval_and_bind, VAR_DEFAULT};
use crate::commands::{CommandContext, ManamiSlashCommand};

pub const SLASH_CALC_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "calc",
    usage: "/calc <expr>",
    description: "数式を計算するよ！",
    register,
    run: |options, ctx| {
        let expr = parse_options(options);
        Box::pin(async move { run_body(expr, &ctx).await })
    },
    is_local_command: false,
};

pub fn register() -> serenity::builder::CreateCommand {
    serenity::builder::CreateCommand::new("calc")
        .description("数式を計算するよ！")
        .add_option(
            serenity::builder::CreateCommandOption::new(CommandOptionType::String, "expr", "数式")
                .required(true),
        )
}

fn parse_options(options: Vec<ResolvedOption<'_>>) -> String {
    options
        .iter()
        .fold(String::new(), |expr, option| match (option.name, &option.value) {
            ("expr", ResolvedValue::String(s)) => (*s).to_owned(),
            _ => expr,
        })
}

async fn run_body(expr: String, ctx: &CommandContext<'_>) -> String {
    eval_and_bind(VAR_DEFAULT, &expr, ctx.bot, ctx.author_id).await
}
