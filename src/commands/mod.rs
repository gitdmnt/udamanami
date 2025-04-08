pub mod calc;
pub mod channel;
pub mod dice;
pub mod help;
pub mod isprime;
pub mod var;
pub mod varbulk;

#[derive(Clone)]
pub struct CommandContext<'a> {
    pub bot: &'a crate::Bot,
    pub http_cache: &'a serenity::http::Http,
    pub channel_id: &'a serenity::model::id::ChannelId,
    pub author_id: &'a serenity::model::id::UserId,
    pub command: String,
}

impl<'a> CommandContext<'a> {
    pub const fn new(
        bot: &'a crate::Bot,
        http_cache: &'a serenity::http::Http,
        msg: &'a serenity::model::channel::Message,
        command: String,
    ) -> Self {
        Self {
            bot,
            http_cache,
            channel_id: &msg.channel_id,
            author_id: &msg.author.id,
            command,
        }
    }

    pub fn command_name(&self) -> String {
        let split_message = self.command.split_whitespace().collect::<Vec<&str>>();
        split_message[0].trim().to_owned()
    }
    pub fn args(&self) -> Vec<&str> {
        let split_message = self.command.split_whitespace().collect::<Vec<&str>>();
        split_message[1..].to_vec()
    }
}
