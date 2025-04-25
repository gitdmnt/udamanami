use crate::commands::CommandContext;

use crate::commands::ManamiPrefixCommand;
pub const PREFIX_CLEAR_COMMAND: ManamiPrefixCommand = ManamiPrefixCommand {
    name: "clear",
    alias: &[
        "forget",
        "全部忘れて",
        "ぜんぶ忘れて",
        "全部わすれて",
        "ぜんぶわすれて",
    ],
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
