use serenity::builder::CreateCommand;

use super::ManamiSlashCommand;

pub const SLASH_PING_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "ping",
    usage: "/ping",
    description: "起きてたらお返事するね！",
    register,
    run: |_, _| Box::pin(async { run() }),
    is_local_command: false,
};

pub fn run() -> String {
    "いるよー！".to_owned()
}

pub fn register() -> CreateCommand {
    CreateCommand::new("ping").description("A ping command")
}
