use serenity::{all::ResolvedOption, builder::CreateCommand};

use super::ManamiSlashCommand;

pub struct SlashCommand;
const COMMAND_NAME: &str = "ping";
impl ManamiSlashCommand for SlashCommand {
    fn name(&self) -> &'static [&'static str] {
        &[COMMAND_NAME]
    }

    fn description(&self) -> &'static str {
        "起きてたらお返事するね！"
    }

    fn register(&self) -> CreateCommand {
        register()
    }

    async fn run(&self, _: &[ResolvedOption<'_>], _: &crate::Bot) -> String {
        run()
    }
}

// ping command
pub fn run() -> String {
    "いるよー！".to_owned()
}

pub fn register() -> CreateCommand {
    CreateCommand::new(COMMAND_NAME).description("A ping command")
}
