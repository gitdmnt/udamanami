use crate::calculator::{eval_from_str, val_as_str};
use crate::commands::CommandContext;

use crate::commands::ManamiPrefixCommand;
use serenity::all::ResolvedOption;
pub struct PrefixCommand;

impl ManamiPrefixCommand for PrefixCommand {
    fn name(&self) -> &'static [&'static str] {
        &["calcsay"]
    }

    fn usage(&self) -> &'static str {
        "!calcsay <expr>"
    }

    fn description(&self) -> &'static str {
        "calcの結果を代筆先に送信するよ！"
    }

    async fn run(&self, ctx: &CommandContext<'_>, _: &[ResolvedOption<'_>]) {
        run(ctx).await
    }

    fn is_dm_command(&self) -> bool {
        true
    }

    fn is_guild_command(&self) -> bool {
        false
    }
}
pub async fn run(ctx: &CommandContext<'_>) {
    let bot = ctx.bot;
    let reply = bot.get_user_room_pointer(ctx.author_id);
    let expression = ctx.args().join(" ");

    let result = eval_from_str(&expression, &bot.variables);
    if let Ok(result) = result {
        reply
            .say(&ctx.cache_http(), val_as_str(&result))
            .await
            .unwrap();
    }
}
