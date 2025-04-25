use crate::commands::CommandContext;

use crate::commands::StManamiPrefixCommand;
pub const CLEAR_COMMAND: StManamiPrefixCommand = StManamiPrefixCommand {
    name: "clear",
    usage: "!clear",
    description: "チャンネルの会話ログを忘れるよ！",
    run: |ctx, _| Box::pin(run(ctx)),
    is_dm_command: false,
    is_guild_command: true,
};

pub async fn run(ctx: CommandContext<'_>) {
    ctx.channel_id
        .say(ctx.cache_http(), "1……2の……ポカン！")
        .await
        .unwrap();

    ctx.bot.gemini.clear();
}
