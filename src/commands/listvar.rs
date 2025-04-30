use crate::commands::var;
use crate::commands::CommandContext;

use crate::commands::ManamiPrefixCommand;

pub const PREFIX_LISTVAR_COMMAND: ManamiPrefixCommand = ManamiPrefixCommand {
    name: "listvar",
    alias: &[],
    usage: "!listvar",
    description: "定義された変数の一覧を表示するよ！",
    run: |ctx| Box::pin(run(ctx)),
    is_dm_command: true,
    is_guild_command: true,
};

pub async fn run(ctx: CommandContext<'_>) {
    var::list_var(ctx.channel_id, ctx.cache_http(), ctx.bot).await;
}
