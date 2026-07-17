use crate::commands::CommandContext;

use crate::commands::{ManamiPrefixCommand, ManamiSlashCommand};

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
    run: |ctx| Box::pin(run_old(ctx)),
    is_dm_command: false,
    is_guild_command: true,
};

pub const SLASH_CLEAR_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "clear",
    usage: "/clear",
    description: "チャンネルの会話ログを忘れるよ！",
    register,
    run: |_, ctx| Box::pin(async move { run(ctx.bot, ctx.channel_id.get()) }),
    is_local_command: true,
};

pub fn register() -> serenity::builder::CreateCommand {
    serenity::builder::CreateCommand::new("clear").description("チャンネルの会話ログを忘れるよ！")
}

fn run(bot: &crate::Bot, channel_id: u64) -> String {
    bot.ai.clear(channel_id);
    "1……2の……ポカン！".to_owned()
}

pub async fn run_old(ctx: CommandContext<'_>) {
    ctx.channel_id
        .say(ctx.cache_http(), "1……2の……ポカン！")
        .await
        .unwrap();

    ctx.bot.ai.clear(ctx.channel_id.get());
}
