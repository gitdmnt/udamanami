use crate::commands::CommandContext;

pub async fn run(ctx: &CommandContext<'_>) {
    ctx.channel_id
        .say(ctx.cache_http(), "1……2の……ポカン！")
        .await
        .unwrap();

    ctx.bot.gemini.clear();
}
