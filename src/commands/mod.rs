pub mod calc;
pub mod calcsay;
pub mod cclemon;
pub mod channel;
pub mod dice;
pub mod help;
pub mod isprime;
pub mod jail;
pub mod unjail;
pub mod var;
pub mod varbulk;

#[derive(Clone)]
pub struct CommandContext<'a> {
    pub bot: &'a crate::Bot,
    pub ctx: &'a serenity::client::Context,
    pub channel_id: &'a serenity::model::id::ChannelId,
    pub author_id: &'a serenity::model::id::UserId,
    pub command: String,
}

impl<'a> CommandContext<'a> {
    pub const fn new(
        bot: &'a crate::Bot,
        ctx: &'a serenity::client::Context,
        msg: &'a serenity::model::channel::Message,
        command: String,
    ) -> Self {
        Self {
            bot,
            ctx,
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
    #[allow(clippy::missing_const_for_fn)]
    pub fn cache_http(&self) -> &'a serenity::http::Http {
        &self.ctx.http
    }
}
