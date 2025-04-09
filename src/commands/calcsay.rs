use crate::calculator::{eval_from_str, val_as_str};
use crate::commands::CommandContext;

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
