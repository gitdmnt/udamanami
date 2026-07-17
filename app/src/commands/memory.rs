use crate::commands::CommandContext;
use crate::commands::ManamiSlashCommand;
use serenity::all::{CommandOptionType, ResolvedOption, ResolvedValue};
use tracing::error;

const USAGE: &str = "/memory [list <page>|get <id>|search <query>]";

pub const SLASH_MEMORY_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "memory",
    usage: USAGE,
    description: "まなみの記憶を覗けるよ！",
    register,
    run: |options, ctx| Box::pin(async move { run(options, ctx).await }),
    is_local_command: true,
};

pub fn register() -> serenity::builder::CreateCommand {
    serenity::builder::CreateCommand::new("memory")
        .description("まなみの記憶を覗けるよ！")
        .add_option(
            serenity::builder::CreateCommandOption::new(
                CommandOptionType::SubCommand,
                "list",
                "記憶の一覧を取得する",
            )
            .add_sub_option(serenity::builder::CreateCommandOption::new(
                CommandOptionType::Integer,
                "page",
                "記憶リストのページ",
            )),
        )
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
        .add_option(
            serenity::builder::CreateCommandOption::new(
                CommandOptionType::SubCommand,
                "search",
                "記憶を検索する",
            )
            .add_sub_option(
                serenity::builder::CreateCommandOption::new(
                    CommandOptionType::String,
                    "query",
                    "検索クエリ",
                )
                .required(true),
            ),
        )
}

pub async fn run(options: Vec<ResolvedOption<'_>>, ctx: CommandContext<'_>) -> String {
    let Some(subcommand) = options.first() else {
        return format!("使い方: {}", USAGE);
    };
    let ResolvedValue::SubCommand(sub_options) = &subcommand.value else {
        return format!("使い方: {}", USAGE);
    };

    match subcommand.name {
        "list" => list(sub_options, &ctx).await,
        "get" => get(sub_options, &ctx).await,
        "search" => search(sub_options, &ctx).await,
        other => format!("「{other}」は知らないサブコマンドだよ。"),
    }
}

async fn list(sub_options: &[ResolvedOption<'_>], ctx: &CommandContext<'_>) -> String {
    let Some(page) = sub_options.first().map(|o| match (o.name, &o.value) {
        ("page", ResolvedValue::Integer(i)) => *i,
        _ => 1,
    }) else {
        return format!("使い方: {}", USAGE);
    };

    if page < 1 {
        return "ページ番号は1以上で指定してね。".to_owned();
    }

    let page = page as usize;

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
        .collect::<Vec<_>>()[((page - 1) * 20)..(page * 20).min(memories.len())]
        .join("\n");
    format!("記憶の一覧だよ:\n{body}")
}

async fn get(sub_options: &[ResolvedOption<'_>], ctx: &CommandContext<'_>) -> String {
    let Some(id) = sub_options.iter().find_map(|o| match (o.name, &o.value) {
        ("id", ResolvedValue::String(s)) => Some(*s),
        _ => None,
    }) else {
        return format!("使い方: {}", USAGE);
    };

    match ctx.bot.database.get_memory(id).await {
        Ok(memory) => format!("**{}**\n{}", memory.title, memory.content),
        Err(e) => {
            error!("Error getting memory {id}: {e:?}");
            format!("ID `{id}` の記憶は取得できなかったよ……")
        }
    }
}

async fn search(sub_options: &[ResolvedOption<'_>], ctx: &CommandContext<'_>) -> String {
    let Some(query) = sub_options.iter().find_map(|o| match (o.name, &o.value) {
        ("query", ResolvedValue::String(s)) => Some(*s),
        _ => None,
    }) else {
        return format!("使い方: {}", USAGE);
    };

    match ctx.bot.database.search_memory(query, 20).await {
        Ok(results) => {
            if results.is_empty() {
                return format!("「{query}」に関する記憶は見つからなかったよ。");
            }

            let body = results
                .iter()
                .map(|r| format!("- `{}` {}", r.memory_id, r.title))
                .collect::<Vec<_>>()
                .join("\n");
            format!("「{query}」に関する記憶だよ:\n{body}")
        }
        Err(e) => {
            error!("Error searching memories for query {query}: {e:?}");
            format!("「{query}」に関する記憶は検索できなかったよ……")
        }
    }
}
