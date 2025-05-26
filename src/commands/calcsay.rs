use crate::calculator::{eval_from_str, val_as_str};
use crate::commands::CommandContext;

use crate::commands::ManamiPrefixCommand;
pub const PREFIX_CALCSAY_COMMAND: ManamiPrefixCommand = ManamiPrefixCommand {
    name: "calcsay",
    alias: &[],
    usage: "!calcsay <expr>",
    description: "calcの結果を代筆先に送信するよ！",
    run: |ctx| Box::pin(run(ctx)),
    is_dm_command: true,
    is_guild_command: false,
};

pub async fn run(ctx: CommandContext<'_>) {
    let bot = ctx.bot;
    let reply = bot.get_user_room_pointer(&ctx.author_id).await;
    let expression = ctx.args().join(" ");

    let result = eval_from_str(&expression, &bot.variables);
    if let Ok(result) = result {
        reply
            .say(&ctx.cache_http(), val_as_str(&result))
            .await
            .unwrap();
    }
}
