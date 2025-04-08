use crate::commands::var::var_main;
use crate::commands::CommandContext;

pub async fn run(ctx: &CommandContext<'_>) {
    let reply = ctx.channel_id;
    let bot = ctx.bot;
    let expression = ctx.args().join(" ");

    var_main(reply, ctx.http_cache, "_".to_owned(), expression, bot).await;
}
