use crate::{commands::CommandContext, Bot};
use serenity::{builder::CreateCommand, utils::MessageBuilder};

use super::{ManamiPrefixCommand, ManamiSlashCommand};

pub const PREFIX_HELP_COMMAND: ManamiPrefixCommand = ManamiPrefixCommand {
    name: "help",
    alias: &["たすけて", "助けて"],
    usage: "!help",
    description: "まなみの自己紹介だよ！",
    run: |ctx| Box::pin(run_old(ctx)),
    is_dm_command: true,
    is_guild_command: true,
};

pub const SLASH_HELP_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "help",
    usage: "/help",
    description: "ヘルプを表示するよ！",
    register,
    run: |_, bot| {
        let result = run(bot);
        Box::pin(async move { result })
    },
    is_local_command: false,
};

const ABOUT_ME: &str = "# まなみの自己紹介だよ！\n";
const ABOUT_GHOSTWRITE: &str = "## 代筆機能があるよ！\nまなみは代筆ができるよ！　DMに送ってもらったメッセージを`!channel`で指定されたチャンネルに転送するよ！\n";
const ABOUT_AI: &str = "## おはなしもできるよ！\nまなみの部屋でいっぱい話しかけてね！\n";

fn generate_help_rows(usages: &[(&str, &str)], usage_space_minimum: usize) -> String {
    let usage_space = usages
        .iter()
        .map(|(usage, _)| usage.len() + 1)
        .max()
        .unwrap_or(0)
        .max(usage_space_minimum);

    usages
        .iter()
        .map(|(usage, description)| format!("{usage:<usage_space$}{description}"))
        .collect::<Vec<_>>()
        .join("\n")
}

const USAGE_SPACE_MINIMUM: usize = 21;

fn generate_slash_help(slash_commands: &[ManamiSlashCommand]) -> String {
    let help_str = generate_help_rows(
        &slash_commands
            .iter()
            .map(|cmd| (cmd.usage, cmd.description))
            .collect::<Vec<_>>(),
        USAGE_SPACE_MINIMUM,
    );
    let mut content = MessageBuilder::new();
    content.push("## まなみはスラッシュコマンドに対応しているよ！\n");
    content.push("```\n");
    content.push(help_str);
    content.push("\n```\n");
    content.build()
}

fn generate_dm_help(prefix_commands: &[ManamiPrefixCommand]) -> String {
    let help_str = generate_help_rows(
        &prefix_commands
            .iter()
            .filter(|cmd| cmd.is_dm_command)
            .map(|cmd| (cmd.usage, cmd.description))
            .collect::<Vec<_>>(),
        USAGE_SPACE_MINIMUM,
    );

    let mut content = MessageBuilder::new();
    content.push("## まなみはDMでコマンドを受け付けるよ！\n");
    content.push("```\n");
    content.push(help_str);
    content.push("\n```\n");
    content.build()
}

fn generate_guild_help(prefix_commands: &[ManamiPrefixCommand]) -> String {
    let help_str = generate_help_rows(
        &prefix_commands
            .iter()
            .filter(|cmd| cmd.is_guild_command)
            .map(|cmd| (cmd.usage, cmd.description))
            .collect::<Vec<_>>(),
        USAGE_SPACE_MINIMUM,
    );

    let mut content = MessageBuilder::new();
    content.push("## まなみはグループチャットでコマンドを受け付けるよ！\n");
    content.push("```\n");
    content.push(help_str);
    content.push("\n```\n");
    content.build()
}

fn generate_help(
    slash_commands: &[ManamiSlashCommand],
    prefix_commands: &[ManamiPrefixCommand],
) -> String {
    let mut content = MessageBuilder::new();
    content
        .push(ABOUT_ME)
        .push(ABOUT_GHOSTWRITE)
        .push(ABOUT_AI)
        .push(generate_slash_help(slash_commands))
        .push(generate_dm_help(prefix_commands))
        .push(generate_guild_help(prefix_commands))
        .build()
}

pub fn register() -> CreateCommand {
    CreateCommand::new("help").description("まなみの自己紹介だよ！")
}

pub fn run(bot: &Bot) -> String {
    generate_help(&bot.slash_commands, &bot.prefix_commands)
}

pub async fn run_old(ctx: CommandContext<'_>) {
    let content = run(ctx.bot);

    ctx.channel_id.say(ctx.cache_http(), content).await.unwrap();
}

#[cfg(test)]
mod tests {
    use crate::{prefix_commands, slash_commands};

    use super::*;

    #[test]
    fn test_generate_help() {
        let slash_commands = slash_commands(&[]);
        let prefix_commands = prefix_commands(&[]);

        let help = generate_help(&slash_commands, &prefix_commands);
        println!("{help}");
        assert!(help.contains("まなみの自己紹介だよ！"));
    }
}
