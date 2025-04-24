use serenity::builder::CreateCommand;

use crate::{commands::ManamiSlashCommand, Bot};
use serenity::model::application::ResolvedOption;
pub struct SlashCommand;

const COMMAND_NAME: &str = "endauto";

impl ManamiSlashCommand for SlashCommand {
    fn name(&self) -> &'static [&'static str] {
        &[COMMAND_NAME]
    }

    fn description(&self) -> &'static str {
        "自動返信を止めるよ！"
    }

    fn register(&self) -> CreateCommand {
        register()
    }

    async fn run(&self, _: &[ResolvedOption<'_>], bot: &Bot) -> String {
        run(bot).await
    }

    fn is_local_command(&self) -> bool {
        true
    }
}

pub fn register() -> CreateCommand {
    CreateCommand::new("endauto").description("自動返信を終了するよ")
}

pub async fn run(bot: &Bot) -> String {
    bot.reply_to_all_mode.lock().unwrap().end();

    "（全レス終了）".to_owned()
}
