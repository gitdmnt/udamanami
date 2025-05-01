use crate::Bot;
use regex::Regex;
use serenity::all::UserId;
use serenity::http::Http;
use serenity::model::id::ChannelId;

use crate::calculator::{self, val_as_str};
use crate::commands::CommandContext;

use super::ManamiPrefixCommand;

pub const PREFIX_VAR_COMMAND: ManamiPrefixCommand = ManamiPrefixCommand {
    name: "var",
    alias: &[],
    usage: "!var <name>=<expr>",
    description: "calcで使える変数を定義するよ！",
    run: |ctx| Box::pin(run(ctx)),
    is_dm_command: true,
    is_guild_command: true,
};

pub const VAR_DEFAULT: &str = "_";

pub async fn run(ctx: CommandContext<'_>) {
    let reply = ctx.channel_id;
    let bot = ctx.bot;
    let input = ctx.args().join(" ");
    var(reply, ctx.cache_http(), input, bot, ctx.author_id).await;
}

pub async fn var(reply: ChannelId, cache_http: &Http, input: String, bot: &Bot, author_id: UserId) {
    let var_pattern = Regex::new(r"([a-zA-Z0-9]+)\s*=\s*(.*)").unwrap();

    let (var, expression) = match var_pattern.captures(&input) {
        Some(caps) => {
            if caps.len() == 3 {
                (
                    caps.get(1).unwrap().as_str().to_owned(),
                    caps.get(2).unwrap().as_str().to_owned(),
                )
            } else {
                (VAR_DEFAULT.to_owned(), input)
            }
        }
        None => (VAR_DEFAULT.to_owned(), input),
    };
    var_main(reply, cache_http, var, expression, bot, author_id).await;
}

pub async fn var_main(
    reply: ChannelId,
    cache_http: &Http,
    var: String,
    expression: String,
    bot: &Bot,
    author_id: UserId,
) {
    let result = calculator::eval_from_str(&expression, &bot.variables);
    match result {
        Ok(result) => {
            if var != VAR_DEFAULT {
                bot.database
                    .upsert_var(&var, result.clone(), author_id)
                    .await
                    .ok();
            }
            bot.variables.insert(var, result.clone());
            reply.say(&cache_http, val_as_str(&result)).await.unwrap();
        }
        Err(e) => {
            reply
                .say(&cache_http, format!("{e} ……だってさ。"))
                .await
                .unwrap();
        }
    }
}

pub async fn delete_var(reply: &ChannelId, cache_http: &Http, var: &String, bot: &Bot) {
    bot.database.delete_var(var).await.ok();
    bot.variables.remove(var);
    reply
        .say(&cache_http, format!("変数 `{var}` を削除したよ！"))
        .await
        .unwrap();
}

pub async fn list_var(reply: ChannelId, cache_http: &Http, bot: &Bot) {
    match bot.database.list_var().await {
        Ok(vars) => {
            let mut result = String::new();
            let spaces = vars.iter().map(|(k, _)| k.len()).max().unwrap_or(0);
            result.push_str("```\n");
            result.push_str(&format!(" {:<spaces$}    author\n", "varname"));
            result.push_str(&format!("─{:─<spaces$}────────────────\n", ""));
            for (key, author) in vars.iter() {
                result.push_str(&format!(" {key:<spaces$}    {author}\n"));
            }
            result.push_str("```");
            if result.is_empty() {
                result = "変数はないよ！".to_owned();
            }
            reply.say(&cache_http, result).await.unwrap();
        }
        Err(e) => {
            reply
                .say(&cache_http, format!("変数の取得に失敗したよ！: {e}"))
                .await
                .unwrap();
        }
    }
}
