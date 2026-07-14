use crate::Bot;
use regex::Regex;
use serenity::all::{CommandOptionType, ResolvedOption, ResolvedValue, UserId};

use crate::calculator::{self, val_as_str};
use crate::commands::{CommandContext, ManamiSlashCommand};

pub const VAR_DEFAULT: &str = "_";

pub const SLASH_VAR_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "var",
    usage: "/var <name> <expr>",
    description: "calcで使える変数を定義するよ！",
    register,
    run: |options, ctx| {
        let (name, expr) = parse_options(options);
        Box::pin(async move { run_body(name, expr, &ctx).await })
    },
    is_local_command: false,
};

pub fn register() -> serenity::builder::CreateCommand {
    serenity::builder::CreateCommand::new("var")
        .description("calcで使える変数を定義するよ！")
        .add_option(
            serenity::builder::CreateCommandOption::new(
                CommandOptionType::String,
                "name",
                "変数名",
            )
            .required(true),
        )
        .add_option(
            serenity::builder::CreateCommandOption::new(CommandOptionType::String, "expr", "式")
                .required(true),
        )
}

fn parse_options(options: Vec<ResolvedOption<'_>>) -> (Option<String>, String) {
    options.iter().fold(
        (None, String::new()),
        |(name, expr), option| match (option.name, &option.value) {
            ("name", ResolvedValue::String(s)) => (Some((*s).to_owned()), expr),
            ("expr", ResolvedValue::String(s)) => (name, (*s).to_owned()),
            _ => (name, expr),
        },
    )
}

async fn run_body(name: Option<String>, expr: String, ctx: &CommandContext<'_>) -> String {
    let name = name.unwrap_or_else(|| VAR_DEFAULT.to_owned());
    eval_and_bind(&name, &expr, ctx.bot, ctx.author_id).await
}

/// `name=expr` 形式（または裸の式）をパースして評価・束縛し、表示用の文字列を返す。
pub async fn set_var(input: &str, bot: &Bot, author_id: UserId) -> String {
    let var_pattern = Regex::new(r"([a-zA-Z0-9]+)\s*=\s*(.*)").unwrap();

    let (var, expression) = var_pattern.captures(input).map_or_else(
        || (VAR_DEFAULT.to_owned(), input.to_owned()),
        |caps| {
            (
                caps.get(1).unwrap().as_str().to_owned(),
                caps.get(2).unwrap().as_str().to_owned(),
            )
        },
    );
    eval_and_bind(&var, &expression, bot, author_id).await
}

/// 式を評価し、`var` が `VAR_DEFAULT` でなければ永続化して束縛する。表示用の文字列を返す。
pub async fn eval_and_bind(var: &str, expression: &str, bot: &Bot, author_id: UserId) -> String {
    match calculator::eval_from_str(expression, &bot.variables) {
        Ok(result) => {
            if var != VAR_DEFAULT {
                bot.database
                    .upsert_var(var, result.clone(), author_id)
                    .await
                    .ok();
            }
            bot.variables.insert(var.to_owned(), result.clone());
            val_as_str(&result)
        }
        Err(e) => format!("{e} ……だってさ。"),
    }
}

pub async fn delete_var(var: &str, bot: &Bot) -> String {
    bot.database.delete_var(var).await.ok();
    bot.variables.remove(&var.to_owned());
    format!("変数 `{var}` を削除したよ！")
}

pub async fn list_var(bot: &Bot) -> String {
    match bot.database.list_var().await {
        Ok(vars) => {
            if vars.is_empty() {
                return "変数はないよ！".to_owned();
            }
            let mut result = String::new();
            let spaces = vars.iter().map(|(k, _)| k.len()).max().unwrap_or(0);
            result.push_str("```\n");
            result.push_str(&format!(" {:<spaces$}    author\n", "varname"));
            result.push_str(&format!("─{:─<spaces$}────────────────\n", ""));
            for (key, author) in &vars {
                result.push_str(&format!(" {key:<spaces$}    {author}\n"));
            }
            result.push_str("```");
            result
        }
        Err(e) => format!("変数の取得に失敗したよ！: {e}"),
    }
}
