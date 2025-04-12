use serenity::builder::CreateCommand;

// ping command
pub fn run() -> String {
    "いるよー！".to_owned()
}

pub fn register() -> CreateCommand {
    CreateCommand::new("ping").description("A ping command")
}
