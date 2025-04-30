use crate::commands::var;
use crate::commands::CommandContext;

use crate::commands::ManamiPrefixCommand;

pub const PREFIX_DELETEVAR_COMMAND: ManamiPrefixCommand = ManamiPrefixCommand {
    name: "deletevar",
    alias: &[],
    usage: "!deletevar varname1 [varname2 varname3 ...]",
    description: "定義した変数を消去するよ！",
    run: |ctx| Box::pin(run(ctx)),
    is_dm_command: true,
    is_guild_command: true,
};

pub async fn run(ctx: CommandContext<'_>) {
    for s in ctx.args().into_iter() {
        if s.trim().is_empty() {
            return;
        }
        var::delete_var(ctx.channel_id, ctx.cache_http(), &s.to_owned(), ctx.bot).await;
    }
}
