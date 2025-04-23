use crate::commands::CommandContext;

use crate::commands::ManamiPrefixCommand;
use serenity::all::ResolvedOption;
pub struct PrefixCommand;

impl ManamiPrefixCommand for PrefixCommand {
    fn name(&self) -> &'static [&'static str] {
        &["clear", "全部忘れて"]
    }

    fn usage(&self) -> &'static str {
        "!clear"
    }

    fn description(&self) -> &'static str {
        "チャンネルの会話ログを忘れるよ！"
    }

    async fn run(&self, ctx: &CommandContext<'_>, _: &[ResolvedOption<'_>]) {
        run(ctx).await
    }

    fn is_dm_command(&self) -> bool {
        false
    }

    fn is_guild_command(&self) -> bool {
        true
    }
}

pub async fn run(ctx: &CommandContext<'_>) {
    ctx.channel_id
        .say(ctx.cache_http(), "1……2の……ポカン！")
        .await
        .unwrap();

    ctx.bot.gemini.clear();
}
