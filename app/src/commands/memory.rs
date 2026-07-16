use crate::commands::CommandContext;
use crate::commands::ManamiSlashCommand;
use serenity::all::{CommandOptionType, ResolvedOption, ResolvedValue};
use tracing::error;

pub const SLASH_CHANNEL_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "memory",
    usage: "/memory [list|get <id>]",
    description: "まなみの記憶を覗けるよ！",
    register,
    run: |options, ctx| Box::pin(async move { run(options, ctx).await }),
    is_local_command: false,
};

pub fn register() -> serenity::builder::CreateCommand {
    serenity::builder::CreateCommand::new("memory")
        .description("まなみの記憶を覗けるよ！")
        .add_option(serenity::builder::CreateCommandOption::new(
            CommandOptionType::SubCommand,
            "list",
            "記憶の一覧を取得する",
        ))
        .add_option(
            serenity::builder::CreateCommandOption::new(
                CommandOptionType::SubCommand,
                "get",
                "指定した記憶を取得する",
            )
            .add_sub_option(
                serenity::builder::CreateCommandOption::new(
                    CommandOptionType::String,
                    "id",
                    "取得する記憶のID",
                )
                .required(true),
            ),
        )
}

pub async fn run(options: Vec<ResolvedOption<'_>>, ctx: CommandContext<'_>) -> String {
    let Some(subcommand) = options.first() else {
        return "使い方: /memory [list|get <id>]".to_owned();
    };
    let ResolvedValue::SubCommand(sub_options) = &subcommand.value else {
        return "使い方: /memory [list|get <id>]".to_owned();
    };

    match subcommand.name {
        "list" => list(&ctx).await,
        "get" => get(sub_options, &ctx).await,
        other => format!("「{other}」は知らないサブコマンドだよ。"),
    }
}

async fn list(ctx: &CommandContext<'_>) -> String {
    let memories = match ctx.bot.database.list_memories().await {
        Ok(memories) => memories,
        Err(e) => {
            error!("Error listing memories: {e:?}");
            return "記憶の一覧を取得できなかったよ……もう一回試してみて".to_owned();
        }
    };

    if memories.is_empty() {
        return "まだ何も記憶してないよ。".to_owned();
    }

    let body = memories
        .iter()
        .map(|m| format!("- `{}` {}", m.memory_id, m.title))
        .collect::<Vec<_>>()
        .join("\n");
    format!("記憶の一覧だよ:\n{body}")
}

async fn get(sub_options: &[ResolvedOption<'_>], ctx: &CommandContext<'_>) -> String {
    let Some(id) = sub_options.iter().find_map(|o| match (o.name, &o.value) {
        ("id", ResolvedValue::String(s)) => Some(*s),
        _ => None,
    }) else {
        return "取得する記憶のIDを指定してね。".to_owned();
    };

    match ctx.bot.database.get_memory(id).await {
        Ok(memory) => format!("**{}**\n{}", memory.title, memory.content),
        Err(e) => {
            error!("Error getting memory {id}: {e:?}");
            format!("ID `{id}` の記憶は取得できなかったよ……")
        }
    }
}
