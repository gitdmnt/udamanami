use serenity::all::{CommandOptionType, ResolvedOption, ResolvedValue};

use crate::calculator::{eval_from_str, val_as_str};
use crate::commands::{CommandContext, ManamiSlashCommand};

pub const SLASH_CALCSAY_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "calcsay",
    usage: "/calcsay <expr>",
    description: "calcの結果を代筆先に送信するよ！",
    register,
    run: |options, ctx| {
        let expr = parse_options(options);
        Box::pin(async move { run_body(expr, &ctx).await })
    },
    is_local_command: false,
};

pub fn register() -> serenity::builder::CreateCommand {
    serenity::builder::CreateCommand::new("calcsay")
        .description("calcの結果を代筆先に送信するよ！")
        .add_option(
            serenity::builder::CreateCommandOption::new(CommandOptionType::String, "expr", "数式")
                .required(true),
        )
}

fn parse_options(options: Vec<ResolvedOption<'_>>) -> String {
    options.iter().fold(String::new(), |expr, option| {
        match (option.name, &option.value) {
            ("expr", ResolvedValue::String(s)) => (*s).to_owned(),
            _ => expr,
        }
    })
}

async fn run_body(expr: String, ctx: &CommandContext<'_>) -> String {
    let bot = ctx.bot;
    let reply = bot.get_user_room_pointer(&ctx.author_id).await;

    match eval_from_str(&expr, &bot.variables) {
        Ok(result) => {
            let result = val_as_str(&result);
            reply.say(ctx.cache_http(), &result).await.ok();
            format!("代筆先に「{result}」を送ったよ")
        }
        Err(e) => format!("{e} ……だってさ。"),
    }
}
