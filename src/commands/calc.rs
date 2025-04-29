use crate::commands::var::{var_main, VAR_DEFAULT};
use crate::commands::CommandContext;

use crate::commands::ManamiPrefixCommand;
pub const PREFIX_CALC_COMMAND: ManamiPrefixCommand = ManamiPrefixCommand {
    name: "calc",
    alias: &[],
    usage: "!calc <expr>",
    description: "数式を計算するよ！",
    run: |ctx| Box::pin(run(ctx)),
    is_dm_command: true,
    is_guild_command: true,
};

pub async fn run(ctx: CommandContext<'_>) {
    let reply = ctx.channel_id;
    let bot = ctx.bot;
    let expression = ctx.args().join(" ");

    var_main(
        reply,
        ctx.cache_http(),
        VAR_DEFAULT.to_owned(),
        expression,
        bot,
    )
    .await;
}
