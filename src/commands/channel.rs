use crate::commands::CommandContext;
use serenity::utils::MessageBuilder;

use crate::commands::ManamiPrefixCommand;
pub const PREFIX_CHANNEL_COMMAND: ManamiPrefixCommand = ManamiPrefixCommand {
    name: "channel",
    alias: &[],
    usage: "!channel",
    description: "代筆先のチャンネルを指定するよ！",
    run: |ctx, _| Box::pin(run(ctx)),
    is_dm_command: true,
    is_guild_command: false,
};

pub async fn run(ctx: CommandContext<'_>) {
    let args = ctx.args();

    // 引数なしの場合はチャンネル一覧を表示
    if args.is_empty() {
        let mut res = MessageBuilder::new();
        res.push("今は")
            .channel(ctx.bot.get_user_room_pointer(ctx.author_id))
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

    ctx.bot
        .change_room_pointer(ctx.author_id, next_pointer)
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
