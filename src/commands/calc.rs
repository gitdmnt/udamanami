use crate::commands::var::var_main;
use crate::commands::CommandContext;

use crate::commands::ManamiPrefixCommand;
use serenity::all::ResolvedOption;
pub struct PrefixCommand;

impl ManamiPrefixCommand for PrefixCommand {
    fn name(&self) -> &'static [&'static str] {
        &["calc"]
    }

    fn usage(&self) -> &'static str {
        "!calc <expr>"
    }

    fn description(&self) -> &'static str {
        "数式を計算するよ！"
    }

    async fn run(&self, ctx: &CommandContext<'_>, _: &[ResolvedOption<'_>]) {
        run(ctx).await
    }

    fn is_dm_command(&self) -> bool {
        true
    }

    fn is_guild_command(&self) -> bool {
        true
    }
}

pub async fn run(ctx: &CommandContext<'_>) {
    let reply = ctx.channel_id;
    let bot = ctx.bot;
    let expression = ctx.args().join(" ");

    var_main(reply, ctx.cache_http(), "_".to_owned(), expression, bot).await;
}
