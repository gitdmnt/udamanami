use crate::Bot;
use regex::Regex;
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
    var(reply, ctx.cache_http(), input, bot).await;
}

pub async fn var(reply: &ChannelId, cache_http: &Http, input: String, bot: &Bot) {
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
    var_main(reply, cache_http, var, expression, bot).await;
}

pub async fn var_main(
    reply: &ChannelId,
    cache_http: &Http,
    var: String,
    expression: String,
    bot: &Bot,
) {
    let result = calculator::eval_from_str(&expression, &bot.variables);
    match result {
        Ok(result) => {
            if var != VAR_DEFAULT {
                bot.database.upsert_var(&var, result.clone()).await.ok();
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
