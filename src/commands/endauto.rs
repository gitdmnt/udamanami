use serenity::builder::CreateCommand;

use crate::{commands::StManamiSlashCommand, Bot};
pub struct SlashCommand;

const COMMAND_NAME: &str = "endauto";

pub const SLASH_ENDAUTO_COMMAND: StManamiSlashCommand = StManamiSlashCommand {
    name: COMMAND_NAME,
    usage: "/endauto",
    description: "自動返信を止めるよ！",
    register,
    run: |_, bot| Box::pin(run(bot)),
    is_local_command: true,
};

pub fn register() -> CreateCommand {
    CreateCommand::new("endauto").description("自動返信を終了するよ")
}

pub async fn run(bot: &Bot) -> String {
    bot.reply_to_all_mode.lock().unwrap().end();

    "（全レス終了）".to_owned()
}
