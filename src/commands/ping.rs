use crate::commands::CommandContext;

// ping command
pub async fn run(ctx: &CommandContext<'_>) {
    ctx.channel_id.say(ctx.cache_http(), "pong").await.unwrap();
}
