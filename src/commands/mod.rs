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
pub mod deletevar;
pub mod dice;
pub mod endauto;
pub mod fetch;
pub mod gemini;
pub mod help;
pub mod isprime;
pub mod jail;
pub mod listvar;
pub mod ping;
pub mod unjail;
pub mod var;
pub mod varbulk;

pub fn slash_commands(disabled_commands: &[&str]) -> Vec<ManamiSlashCommand> {
    [
        help::SLASH_HELP_COMMAND,
        ping::SLASH_PING_COMMAND,
        bf::SLASH_BF_COMMAND,
        auto::SLASH_AUTO_COMMAND,
        endauto::SLASH_ENDAUTO_COMMAND,
        gemini::SLASH_GEMINI_COMMAND,
    ]
    .into_iter()
    .filter(|command| !disabled_commands.contains(&command.name))
    .collect::<Vec<_>>()
}

pub fn prefix_commands(disabled_commands: &[&str]) -> Vec<ManamiPrefixCommand> {
    [
        help::PREFIX_HELP_COMMAND,
        dice::PREFIX_DICE_COMMAND,
        isprime::PREFIX_ISPRIME_COMMAND,
        channel::PREFIX_CHANNEL_COMMAND,
        clear::PREFIX_CLEAR_COMMAND,
        deletevar::PREFIX_DELETEVAR_COMMAND,
        jail::PREFIX_JAIL_COMMAND,
        listvar::PREFIX_LISTVAR_COMMAND,
        unjail::PREFIX_UNJAIL_COMMAND,
        cclemon::PREFIX_CCLEMON_COMMAND,
        calc::PREFIX_CALC_COMMAND,
        calcsay::PREFIX_CALCSAY_COMMAND,
        var::PREFIX_VAR_COMMAND,
        varbulk::PREFIX_VARBULK_COMMAND,
        fetch::PREFIX_FETCH_COMMAND,
    ]
    .into_iter()
    .filter(|command| !disabled_commands.contains(&command.name))
    .collect::<Vec<_>>()
}

#[derive(Clone)]
pub struct CommandContext<'a> {
    pub bot: &'a crate::Bot,
    pub ctx: &'a serenity::client::Context,
    pub channel_id: serenity::model::id::ChannelId,
    pub author_id: serenity::model::id::UserId,
    pub command: String,
    pub guild_id: Option<serenity::model::id::GuildId>,
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
            channel_id: msg.channel_id,
            author_id: msg.author.id,
            command,
            guild_id: msg.guild_id,
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
            channel_id: interaction.channel_id,
            author_id: interaction.user.id,
            command: command.to_owned(),
            guild_id: interaction.guild_id,
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
    pub run: for<'a> fn(Vec<ResolvedOption<'a>>, &'a Bot) -> BoxedFuture<'a, String>,
    pub is_local_command: bool,
}
