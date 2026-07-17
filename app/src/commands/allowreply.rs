use serenity::{
    builder::{CreateCommand, CreateCommandOption},
    model::application::{CommandOptionType, ResolvedOption, ResolvedValue},
};
use tracing::error;

use crate::commands::{CommandContext, ManamiSlashCommand};

const DESC: &str = "まなみがこのチャンネルで自分から話に参加する確率を設定するよ！";

pub const SLASH_ALLOWREPLY_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "allowreply",
    usage: "/allowreply [rate]",
    description: DESC,
    register,
    run: |options, ctx| {
        let rate = parse(options);
        Box::pin(async move { run_body(rate, ctx).await })
    },
    is_local_command: true,
};

pub fn register() -> CreateCommand {
    CreateCommand::new("allowreply")
        .description(DESC)
        .add_option(
            CreateCommandOption::new(
                CommandOptionType::Integer,
                "rate",
                "反応確率(%). 未設定の場合はトグルで切り替え",
            )
            .required(false)
            .min_int_value(0)
            .max_int_value(100),
        )
}

fn parse(options: Vec<ResolvedOption<'_>>) -> Option<u32> {
    options
        .iter()
        .fold(None, |rate, option| match (option.name, &option.value) {
            ("rate", ResolvedValue::Integer(v)) => u32::try_from(*v).ok(),
            _ => rate,
        })
}

const FAILED_TO_SET: &str = "設定に失敗しちゃった……もう一回試してみて";

/// 割合の指定があれば「反応ON + その割合」に設定し、無ければ許可をトグルする。
async fn run_body(rate: Option<u32>, ctx: CommandContext<'_>) -> String {
    let channel_id = ctx.channel_id;
    let db = &ctx.bot.database;

    if let Some(rate) = rate {
        return match db
            .set_channel_reply_setting(&channel_id, Some(true), Some(rate))
            .await
        {
            Ok(()) => format!("うん！　これからは{rate}%くらいの確率でおはなしするね！"),
            Err(e) => {
                error!("Error setting channel reply setting: {e:?}");
                FAILED_TO_SET.to_owned()
            }
        };
    }

    // 未設定のチャンネルは debug チャンネルのみ許可済みとみなす(反応判定側と同じ既定)。
    let setting = match db.fetch_channel_reply_setting(&channel_id).await {
        Ok(setting) => setting,
        Err(e) => {
            error!("Error fetching channel reply setting: {e:?}");
            return "設定の取得に失敗しちゃった……もう一回試してみて".to_owned();
        }
    };
    let is_debug_channel = channel_id.get() == ctx.bot.debug_channel_id.get();
    let (enabled, rate) = crate::resolve_reply_setting(setting.as_ref(), is_debug_channel);
    let new_enabled = !enabled;

    // 割合は None で送り、既存の値を据え置く。
    match db
        .set_channel_reply_setting(&channel_id, Some(new_enabled), None)
        .await
    {
        Ok(()) if new_enabled => {
            format!("うん！　これからは{rate}%くらいの確率でおはなしするね！")
        }
        Ok(()) => "まなみには内緒のおはなしなの？　わかったよ、もー……".to_owned(),
        Err(e) => {
            error!("Error setting channel reply setting: {e:?}");
            FAILED_TO_SET.to_owned()
        }
    }
}
