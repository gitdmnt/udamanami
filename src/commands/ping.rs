use serenity::builder::CreateCommand;

// ping command
pub async fn run() -> String {
    "いるよー！".to_owned()
}

pub fn register() -> CreateCommand {
    CreateCommand::new("ping")
        .name_localized("ja-JP", "いる？")
        .description("A ping command")
        .default_member_permissions(serenity::model::Permissions::SEND_MESSAGES)
}
