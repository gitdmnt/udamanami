use crate::commands::var::var_main;
use crate::commands::CommandContext;

use crate::commands::StManamiPrefixCommand;
pub const CALC_COMMAND: StManamiPrefixCommand = StManamiPrefixCommand {
    name: "calc",
    usage: "!calc <expr>",
    description: "数式を計算するよ！",
    run: |ctx, _| Box::pin(run(ctx)),
    is_dm_command: true,
    is_guild_command: true,
};

pub async fn run(ctx: CommandContext<'_>) {
    let reply = ctx.channel_id;
    let bot = ctx.bot;
    let expression = ctx.args().join(" ");

    var_main(reply, ctx.cache_http(), "_".to_owned(), expression, bot).await;
}
