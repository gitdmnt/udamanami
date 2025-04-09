use crate::commands::CommandContext;

pub async fn run(ctx: &CommandContext<'_>) {
    ctx.channel_id
        .say(ctx.cache_http(), "1……2の……ポカン！")
        .await
        .unwrap();

    if let Some(reflog) = ctx.bot.chat_log.get(ctx.channel_id) {
        if let Ok(mut chat_log) = reflog.lock() {
            chat_log.clear();
        }
    }
}
