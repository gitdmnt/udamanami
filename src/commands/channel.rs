use crate::commands::CommandContext;
use crate::commands::ManamiSlashCommand;
use serenity::all::ResolvedValue;

pub const SLASH_CHANNEL_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "channel",
    usage: "/channel <channel>",
    description: "代筆先のチャンネルを指定するよ！",
    register,
    run: |options, ctx| Box::pin(async move { run(options, ctx).await }),
    is_local_command: false,
};

pub fn register() -> serenity::builder::CreateCommand {
    serenity::builder::CreateCommand::new("channel")
        .description("代筆先のチャンネルを指定するよ！")
        .add_option(
            serenity::builder::CreateCommandOption::new(
                serenity::model::application::CommandOptionType::Channel,
                "channel",
                "代筆先のチャンネルを指定するよ！",
            )
            .required(true),
        )
}

pub async fn run(
    options: Vec<serenity::all::ResolvedOption<'_>>,
    ctx: CommandContext<'_>,
) -> String {
    let ResolvedValue::Channel(channel) = options[0].value else {
        let now = format!(
            "今は{}で代筆してるよ",
            ctx.bot.get_user_room_pointer(&ctx.author_id).await
        );
        return now;
    };

    let user = &ctx.author_id.to_user(ctx.cache_http()).await.unwrap();
    let name = user
        .nick_in(ctx.cache_http(), ctx.bot.guild_id)
        .await
        .unwrap_or_else(|| user.display_name().to_owned());

    let msg = format!("送信先を{}に設定したよ", channel.name.as_ref().unwrap());
    ctx.bot
        .change_room_pointer(&ctx.author_id, &name, channel.id)
        .await
        .unwrap();
    msg
}

use crate::commands::ManamiPrefixCommand;
use serenity::utils::MessageBuilder;

pub const PREFIX_CHANNEL_COMMAND: ManamiPrefixCommand = ManamiPrefixCommand {
    name: "channel",
    alias: &[],
    usage: "!channel",
    description: "代筆先のチャンネルを指定するよ！",
    run: |ctx| Box::pin(run_old(ctx)),
    is_dm_command: true,
    is_guild_command: false,
};

pub async fn run_old(ctx: CommandContext<'_>) {
    let args = ctx.args();

    // 引数なしの場合はチャンネル一覧を表示
    if args.is_empty() {
        let mut res = MessageBuilder::new();
        res.push("今は")
            .channel(ctx.bot.get_user_room_pointer(&ctx.author_id).await)
            .push("で代筆してるよ\n")
            .push("```チャンネル一覧だよ\n");
        for (i, ch) in ctx.bot.channel_ids.iter().enumerate() {
            res.push(format!("{i:>2}\t"))
                .push(ch.name(ctx.cache_http()).await.unwrap())
                .push("\n");
        }
        let res = res.push("```").push("使い方: `!channel <ID>`").build();

        ctx.channel_id.say(&ctx.cache_http(), &res).await.unwrap();
        return;
    }

    // それ以外の場合は指定されたチャンネルに切り替え
    let Ok(selector) = args[0].parse::<usize>() else {
        ctx.channel_id
            .say(&ctx.cache_http(), "IDは数字で指定してね")
            .await
            .unwrap();
        return;
    };
    let Some(&next_pointer) = ctx.bot.channel_ids.get(selector) else {
        ctx.channel_id
            .say(&ctx.cache_http(), "しらないチャンネルだよ")
            .await
            .unwrap();
        return;
    };

    let user = &ctx.author_id.to_user(ctx.cache_http()).await.unwrap();
    let name = user
        .nick_in(ctx.cache_http(), ctx.bot.guild_id)
        .await
        .unwrap_or_else(|| user.display_name().to_owned());

    ctx.bot
        .change_room_pointer(&ctx.author_id, &name, next_pointer)
        .await
        .unwrap();
    ctx.channel_id
        .say(
            &ctx.cache_http(),
            MessageBuilder::new()
                .push("送信先を")
                .channel(next_pointer)
                .push("に設定したよ")
                .build(),
        )
        .await
        .unwrap();
}
