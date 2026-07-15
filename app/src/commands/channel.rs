use crate::commands::CommandContext;
use crate::commands::ManamiSlashCommand;
use serenity::all::ResolvedValue;
use tracing::error;

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

    let channel_name = channel.name.as_deref().unwrap_or("");
    if let Err(e) = ctx
        .bot
        .change_room_pointer(&ctx.author_id, &name, channel.id, channel_name)
        .await
    {
        error!("Error changing room pointer: {e:?}");
        return "送信先の設定に失敗しちゃった……もう一回試してみて".to_owned();
    }
    format!("送信先を{channel_name}に設定したよ")
}
