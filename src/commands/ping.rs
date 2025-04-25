use serenity::builder::CreateCommand;

use super::StManamiSlashCommand;

pub const PING_COMMAND: StManamiSlashCommand = StManamiSlashCommand {
    name: "ping",
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
