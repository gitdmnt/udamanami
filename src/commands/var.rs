use crate::Bot;
use serenity::all::ResolvedOption;
use serenity::http::Http;
use serenity::model::id::ChannelId;

use crate::calculator::{self, val_as_str};
use crate::commands::CommandContext;

use super::ManamiPrefixCommand;

pub struct PrefixCommand;
impl ManamiPrefixCommand for PrefixCommand {
    fn name(&self) -> &'static [&'static str] {
        &["var"]
    }

    fn usage(&self) -> &'static str {
        "!var <name>=<expr>"
    }

    fn description(&self) -> &'static str {
        "calcで使える変数を定義するよ！"
    }

    async fn run(&self, ctx: &CommandContext<'_>, _: &[ResolvedOption<'_>]) {
        run(ctx).await
    }

    fn is_dm_command(&self) -> bool {
        true
    }

    fn is_guild_command(&self) -> bool {
        true
    }

    fn is_enabled(&self, disabled_commands: &[&str]) -> bool {
        for name in self.name() {
            if disabled_commands.contains(name) {
                return false;
            }
        }
        true
    }
}

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
    var_main(reply, ctx.cache_http(), var, expression, bot).await;
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
            bot.variables.insert(var, result.clone());
            reply.say(&cache_http, val_as_str(&result)).await.unwrap();
        }
        Err(e) => {
            reply
                .say(&cache_http, format!("{} ……だってさ。", e))
                .await
                .unwrap();
        }
    }
}
