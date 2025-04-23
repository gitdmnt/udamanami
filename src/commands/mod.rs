use crate::Bot;
use serenity::all::ResolvedOption;

pub mod auto;
pub mod bf;
pub mod calc;
pub mod calcsay;
pub mod cclemon;
pub mod channel;
pub mod clear;
pub mod dice;
pub mod endauto;
pub mod gemini;
pub mod help;
pub mod isprime;
pub mod jail;
pub mod ping;
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

    pub fn new_from_command_interaction(
        bot: &'a crate::Bot,
        ctx: &'a serenity::client::Context,
        interaction: &'a serenity::model::application::CommandInteraction,
        command: &'a str,
    ) -> Self {
        Self {
            bot,
            ctx,
            channel_id: &interaction.channel_id,
            author_id: &interaction.user.id,
            command: command.to_owned(),
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

pub trait ManamiPrefixCommand {
    fn name(&self) -> &'static [&'static str];
    fn usage(&self) -> &'static str;
    fn description(&self) -> &'static str;
    fn run(
        &self,
        ctx: &CommandContext<'_>,
        options: &[ResolvedOption],
    ) -> impl std::future::Future<Output = ()> + Send;

    fn is_dm_command(&self) -> bool;
    fn is_guild_command(&self) -> bool;

    fn is_enabled(&self, disabled_commands: &[&str]) -> bool {
        for name in self.name() {
            if disabled_commands.contains(name) {
                return false;
            }
        }
        true
    }
}
pub trait ManamiSlashCommand {
    fn name(&self) -> &'static [&'static str];
    fn description(&self) -> &'static str;
    fn register(&self) -> serenity::builder::CreateCommand;
    fn run(
        &self,
        option: &[ResolvedOption<'_>],
        bot: &Bot,
    ) -> impl std::future::Future<Output = String> + Send;

    fn try_register(&self, disabled_commands: &[&str]) -> Option<serenity::builder::CreateCommand> {
        if self.is_enabled(disabled_commands) {
            Some(self.register())
        } else {
            None
        }
    }

    fn is_enabled(&self, disabled_commands: &[&str]) -> bool {
        for name in self.name() {
            if disabled_commands.contains(name) {
                return false;
            }
        }
        true
    }
}
