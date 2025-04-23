use crate::cclemon;
use crate::commands::CommandContext;
use serenity::utils::parse_user_mention;

use crate::commands::ManamiPrefixCommand;
use serenity::all::ResolvedOption;
pub struct PrefixCommand;

impl ManamiPrefixCommand for PrefixCommand {
    fn name(&self) -> &'static [&'static str] {
        &["cclemon"]
    }

    fn usage(&self) -> &'static str {
        "!cclemon <opponent>"
    }

    fn description(&self) -> &'static str {
        "CCレモンをするよ！"
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
    let [opponent_id] = ctx.args()[..] else {
        ctx.channel_id
            .say(ctx.cache_http(), "使い方: `!cclemon <相手>`")
            .await
            .unwrap();
        return;
    };
    let Some(opponent_id) = parse_user_mention(opponent_id) else {
        ctx.channel_id
            .say(ctx.cache_http(), "相手をメンションで指定してね")
            .await
            .unwrap();
        return;
    };

    cclemon::cclemon(ctx, (ctx.author_id, &opponent_id)).await;
}
