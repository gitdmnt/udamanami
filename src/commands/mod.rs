use std::pin::Pin;

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

type BoxedFuture<'x, T> = Pin<Box<dyn std::future::Future<Output = T> + Send + 'x>>;

pub struct ManamiPrefixCommand {
    pub name: &'static str,
    pub alias: &'static [&'static str],
    pub usage: &'static str,
    pub description: &'static str,
    pub run: for<'a> fn(CommandContext<'a>) -> BoxedFuture<'a, ()>,
    pub is_dm_command: bool,
    pub is_guild_command: bool,
}

pub struct ManamiSlashCommand {
    pub name: &'static str,
    pub usage: &'static str,
    pub description: &'static str,
    pub register: fn() -> serenity::builder::CreateCommand,
    pub run: for<'a> fn(Vec<ResolvedOption>, &'a Bot) -> BoxedFuture<'a, String>,
    pub is_local_command: bool,
}
