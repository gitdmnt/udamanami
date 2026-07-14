use regex::Regex;
use serenity::all::{CommandOptionType, ResolvedOption, ResolvedValue};

use crate::commands::var;
use crate::commands::{CommandContext, ManamiSlashCommand};

pub const SLASH_VARBULK_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "varbulk",
    usage: "/varbulk <definitions>",
    description: ";区切りで複数の変数を一度に定義するよ！",
    register,
    run: |options, ctx| {
        let input = parse_options(options);
        Box::pin(async move { run_body(input, &ctx).await })
    },
    is_local_command: false,
};

pub fn register() -> serenity::builder::CreateCommand {
    serenity::builder::CreateCommand::new("varbulk")
        .description(";区切りで複数の変数を一度に定義するよ！")
        .add_option(
            serenity::builder::CreateCommandOption::new(
                CommandOptionType::String,
                "definitions",
                "name=expr を ; で区切って並べてね（コードブロックも可）",
            )
            .required(true),
        )
}

fn parse_options(options: Vec<ResolvedOption<'_>>) -> String {
    options
        .iter()
        .fold(String::new(), |input, option| match (option.name, &option.value) {
            ("definitions", ResolvedValue::String(s)) => (*s).to_owned(),
            _ => input,
        })
}

async fn run_body(input: String, ctx: &CommandContext<'_>) -> String {
    // コードブロックで囲まれている場合は中身を取り出す。なければそのまま使う。
    let code_pattern = Regex::new(r"```[a-zA-Z0-9]*(.*)```").unwrap();
    let input = code_pattern
        .captures(&input)
        .map(|caps| caps.get(1).unwrap().as_str().to_owned())
        .unwrap_or(input);

    let mut results = Vec::new();
    for s in input.split(';') {
        if s.trim().is_empty() {
            continue;
        }
        results.push(var::set_var(s, ctx.bot, ctx.author_id).await);
    }

    if results.is_empty() {
        "定義する変数がなかったよ".to_owned()
    } else {
        results.join("\n")
    }
}
