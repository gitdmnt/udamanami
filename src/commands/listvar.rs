use crate::commands::var;
use crate::commands::{CommandContext, ManamiSlashCommand};

pub const SLASH_LISTVAR_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "listvar",
    usage: "/listvar",
    description: "定義された変数の一覧を表示するよ！",
    register,
    run: |_, ctx| Box::pin(async move { run_body(&ctx).await }),
    is_local_command: false,
};

pub fn register() -> serenity::builder::CreateCommand {
    serenity::builder::CreateCommand::new("listvar").description("定義された変数の一覧を表示するよ！")
}

async fn run_body(ctx: &CommandContext<'_>) -> String {
    var::list_var(ctx.bot).await
}
