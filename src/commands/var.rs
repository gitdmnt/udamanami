use crate::Bot;
use serenity::http::Http;
use serenity::model::id::ChannelId;

use crate::calculator::{self, val_as_str};
use crate::commands::CommandContext;

const VAR_DEFAULT: &str = "_";

pub async fn run(ctx: &CommandContext<'_>) {
    let reply = ctx.channel_id;
    let bot = ctx.bot;
    let input = ctx.args().join(" ");

    let split: Vec<&str> = input.split('=').collect();
    let (var, expression) = if split.len() < 2 {
        (VAR_DEFAULT.to_owned(), input)
    } else {
        (split[0].trim().to_owned(), split[1..].join("="))
    };
    var_main(reply, ctx.http_cache, var, expression, bot).await;
}

pub async fn var_main(
    reply: &ChannelId,
    http_cache: &Http,
    var: String,
    expression: String,
    bot: &Bot,
) {
    let result = calculator::eval_from_str(&expression, &bot.variables);
    match result {
        Ok(result) => {
            bot.variables.insert(var, result.clone());
            reply.say(&http_cache, val_as_str(&result)).await.unwrap();
        }
        Err(e) => {
            reply
                .say(&http_cache, format!("{} ……だってさ。", e))
                .await
                .unwrap();
        }
    }
}
