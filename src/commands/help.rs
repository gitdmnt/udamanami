use crate::{commands::CommandContext, Bot};
use serenity::{all::ResolvedOption, builder::CreateCommand, utils::MessageBuilder};

use super::{ManamiPrefixCommand, ManamiSlashCommand};

const ABOUT_ME: &str = "# まなみの自己紹介だよ！\n";
const ABOUT_GHOSTWRITE: &str = "## 代筆機能があるよ！\nまなみは代筆ができるよ！　DMに送ってもらったメッセージを`!channel`で指定されたチャンネルに転送するよ！\n";
const ABOUT_AI: &str = "## おはなしもできるよ！\nまなみの部屋でいっぱい話しかけてね！\n";
const ABOUT_SLASH: &str = "## まなみはスラッシュコマンドに対応しているよ！
```
/help                ヘルプを表示するよ！
/ping                起きてたらお返事するね！
/dice                サイコロを振るよ！　ex. 2d6 <= 9
/bf                  まなみはいんたぷりた？　なんだよ！
```
";

const ABOUT_DM: &str = "## まなみはDMでコマンドを受け付けるよ！
```
!channel             代筆先のチャンネルを指定するよ！
!erocheck            あなたがエロガキかどうかを判定するよ！
!help                ヘルプを表示するよ！
!calc <expr>         数式を計算するよ！
!var <name>=<expr>   calcで使える変数を定義するよ！
!varbulk <codeblock> ;区切りで複数の変数を一度に定義するよ！
!calcsay <expr>      calcの結果を代筆先に送信するよ！
";

const ABOUT_GUILD: &str = "## まなみはグループチャットでコマンドを受け付けるよ！
```
![n]d<m>             m面ダイスをn回振るよ！
!help                ヘルプを表示するよ！
!isprime <n>         nが素数かどうかを判定するよ！
!calc <expr>         数式を計算するよ！
!var <name>=<expr>   calcで使える変数を定義するよ！
!varbulk <codeblock> ;区切りで複数の変数を一度に定義するよ！
!jail <user> [sec]   不届き者を収監して 見せます・袋とじ・管理 以外のカテゴリで喋れなくするよ！
!unjail <user>       収監を解除するよ！
!cclemon <opponent>  CCレモンをするよ！
!clear               コマンドを実行したチャンネルのログを忘れるよ！
```
";

fn generate_help_str(usages: &[(&str, &str)], usage_space_minimum: usize) -> String {
    let usage_space = usages
        .iter()
        .map(|(usage, _)| usage.len() + 1)
        .max()
        .unwrap_or(0)
        .max(usage_space_minimum);

    usages
        .iter()
        .map(|(usage, description)| {
            format!("{:<width$}{}", usage, description, width = usage_space)
        })
        .collect::<Vec<_>>()
        .join("\n")
}

const USAGE_SPACE_MINIMUM: usize = 21;

fn generate_slash_help(slash_commands: &[&impl ManamiSlashCommand]) -> String {
    let usages = &slash_commands
        .iter()
        .map(|cmd| (format!("/{}", cmd.name()[0]), cmd.description()))
        .collect::<Vec<_>>();
    let help_str = generate_help_str(
        &usages
            .iter()
            .map(|(name, desc)| (name.as_str(), *desc))
            .collect::<Vec<_>>(),
        USAGE_SPACE_MINIMUM,
    );

    let mut content = MessageBuilder::new();
    content.push("## まなみはスラッシュコマンドに対応しているよ！\n");
    content.push("```\n");
    content.push(help_str);
    content.push("\n```");
    content.build()
}

fn generate_dm_help(prefix_commands: &[&impl ManamiPrefixCommand]) -> String {
    let help_str = generate_help_str(
        &prefix_commands
            .iter()
            .filter(|cmd| cmd.is_dm_command())
            .map(|cmd| (cmd.usage(), cmd.description()))
            .collect::<Vec<_>>(),
        USAGE_SPACE_MINIMUM,
    );

    let mut content = MessageBuilder::new();
    content.push("## まなみはDMでコマンドを受け付けるよ！\n");
    content.push("```\n");
    content.push(help_str);
    content.push("\n```");
    content.build()
}

fn generate_guild_help(prefix_commands: &[&impl ManamiPrefixCommand]) -> String {
    let help_str = generate_help_str(
        &prefix_commands
            .iter()
            .filter(|cmd| cmd.is_guild_command())
            .map(|cmd| (cmd.usage(), cmd.description()))
            .collect::<Vec<_>>(),
        USAGE_SPACE_MINIMUM,
    );

    let mut content = MessageBuilder::new();
    content.push("## まなみはグループチャットでコマンドを受け付けるよ！\n");
    content.push("```\n");
    content.push(help_str);
    content.push("\n```");
    content.build()
}

pub struct HelpCommand {
    pub message: String,
}
impl HelpCommand {
    pub fn new(
        prefix_commands: &[&impl ManamiPrefixCommand],
        slash_commands: &[&impl ManamiSlashCommand],
    ) -> Self {
        let mut content = MessageBuilder::new();
        content.push(run());
        content.push(generate_slash_help(slash_commands));
        content.push(generate_dm_help(prefix_commands));
        content.push(generate_guild_help(prefix_commands));
        Self {
            message: content.build(),
        }
    }

    pub fn get_prefix_command(&self) -> impl ManamiPrefixCommand {
        PrefixCommand {
            message: self.message.clone(),
        }
    }

    pub fn get_slash_command(&self) -> impl ManamiSlashCommand {
        SlashCommand {
            message: self.message.clone(),
        }
    }
}

struct PrefixCommand {
    pub message: String,
}
struct SlashCommand {
    pub message: String,
}

impl ManamiPrefixCommand for PrefixCommand {
    fn name(&self) -> &'static [&'static str] {
        &["help"]
    }

    fn usage(&self) -> &'static str {
        "!help"
    }

    fn description(&self) -> &'static str {
        "まなみの自己紹介だよ！"
    }

    async fn run(&self, ctx: &CommandContext<'_>, _: &[ResolvedOption<'_>]) {
        ctx.channel_id
            .say(ctx.cache_http(), &self.message)
            .await
            .unwrap();
    }

    fn is_dm_command(&self) -> bool {
        true
    }

    fn is_guild_command(&self) -> bool {
        true
    }
}

const COMMAND_NAME: &str = "help";
impl ManamiSlashCommand for SlashCommand {
    fn name(&self) -> &'static [&'static str] {
        &[COMMAND_NAME]
    }

    fn description(&self) -> &'static str {
        "ヘルプを表示するよ！"
    }

    fn register(&self) -> serenity::builder::CreateCommand {
        register()
    }

    async fn run(&self, _: &[ResolvedOption<'_>], _: &Bot) -> String {
        self.message.clone()
    }

    fn is_local_command(&self) -> bool {
        false
    }
}

pub fn register() -> CreateCommand {
    CreateCommand::new(COMMAND_NAME).description("まなみの自己紹介だよ！")
}

pub fn run() -> String {
    let mut content = MessageBuilder::new();
    content
        .push(ABOUT_ME)
        .push(ABOUT_GHOSTWRITE)
        .push(ABOUT_AI)
        .push(ABOUT_SLASH)
        .push(ABOUT_DM)
        .push(ABOUT_GUILD);
    content.build()
}

pub async fn run_old(ctx: &CommandContext<'_>) {
    let content = run();

    ctx.channel_id.say(ctx.cache_http(), content).await.unwrap();
}
