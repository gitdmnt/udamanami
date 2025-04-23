use serenity::all::ResolvedOption;

use crate::commands::var::var_main;
use crate::commands::CommandContext;

use crate::commands::ManamiPrefixCommand;

pub struct PrefixCommand;

const COMMAND_NAME: &str = "calc";

impl ManamiPrefixCommand for PrefixCommand {
    fn name(&self) -> &'static [&'static str] {
        &[COMMAND_NAME]
    }

    fn description(&self) -> &'static str {
        "数式を計算するよ！"
    }

    fn usage(&self) -> &'static str {
        "!calc <expr>"
    }

    async fn run(&self, ctx: &CommandContext<'_>, _: &[ResolvedOption<'_>]) {
        run(ctx).await
    }
}

pub async fn run(ctx: &CommandContext<'_>) {
    let reply = ctx.channel_id;
    let bot = ctx.bot;
    let expression = ctx.args().join(" ");

    var_main(reply, ctx.cache_http(), "_".to_owned(), expression, bot).await;
}
