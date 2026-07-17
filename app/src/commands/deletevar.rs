use serenity::all::{CommandOptionType, ResolvedOption, ResolvedValue};

use crate::commands::var;
use crate::commands::{CommandContext, ManamiSlashCommand};

pub const SLASH_DELETEVAR_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "deletevar",
    usage: "/deletevar <names>",
    description: "定義した変数を消去するよ！",
    register,
    run: |options, ctx| {
        let names = parse_options(options);
        Box::pin(async move { run_body(names, &ctx).await })
    },
    is_local_command: false,
};

pub fn register() -> serenity::builder::CreateCommand {
    serenity::builder::CreateCommand::new("deletevar")
        .description("定義した変数を消去するよ！")
        .add_option(
            serenity::builder::CreateCommandOption::new(
                CommandOptionType::String,
                "names",
                "消去する変数名（空白区切りで複数指定できるよ）",
            )
            .required(true),
        )
}

fn parse_options(options: Vec<ResolvedOption<'_>>) -> String {
    options.iter().fold(String::new(), |names, option| {
        match (option.name, &option.value) {
            ("names", ResolvedValue::String(s)) => (*s).to_owned(),
            _ => names,
        }
    })
}

async fn run_body(names: String, ctx: &CommandContext<'_>) -> String {
    let mut results = Vec::new();
    for name in names.split_whitespace() {
        results.push(var::delete_var(name, ctx.bot).await);
    }
    if results.is_empty() {
        "消去する変数名を指定してね".to_owned()
    } else {
        results.join("\n")
    }
}
