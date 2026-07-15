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
