pub mod help;

pub struct CommandContext<'a> {
    pub http: &'a serenity::http::Http,
    pub msg: &'a serenity::model::channel::Message,
    pub guild_id: Option<serenity::model::id::GuildId>,
    pub channel_id: &'a serenity::model::id::ChannelId,
    pub user_id: &'a serenity::model::id::UserId,
    pub args: Vec<String>,
}
