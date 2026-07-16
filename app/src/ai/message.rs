//! プロバイダ非依存のチャットメッセージ表現と、rig 形式への変換。

use rig::completion::Message as RigMessage;

/// メッセージの発言者。会話バッファと DB 由来のログで使う。
#[derive(Clone)]
pub enum Role {
    User { name: String },
    Assistant,
}

/// プロバイダ非依存のチャットメッセージ。会話バッファと DB 由来のログで使う。
#[derive(Clone)]
pub struct ChatMessage {
    pub role: Role,
    pub content: String,
}

impl ChatMessage {
    /// ユーザー発言。
    pub fn user(user_name: &str, message: &str) -> Self {
        Self {
            role: Role::User {
                name: user_name.to_owned(),
            },
            content: message.to_owned(),
        }
    }

    /// まなみ（アシスタント）の発言。
    pub fn assistant(message: &str) -> Self {
        Self {
            role: Role::Assistant,
            content: message.to_owned(),
        }
    }

    /// rig のメッセージ形式へ変換する。ユーザー発言は本文の先頭に話者名を付与する。
    pub(crate) fn to_rig(&self) -> RigMessage {
        match &self.role {
            Role::User { name } => RigMessage::user(format!("{}: {}", name, self.content)),
            Role::Assistant => RigMessage::assistant(self.content.clone()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn chat_message_roles() {
        // ユーザー発言は話者名を Role に保持し、content は本文そのもの。
        // 話者名の付与は LLM 送信時（to_rig）に行う。
        let user_msg = ChatMessage::user("宇田", "やあ");
        match &user_msg.role {
            Role::User { name } => assert_eq!(name.as_str(), "宇田"),
            Role::Assistant => panic!("expected a user role"),
        }
        assert_eq!(user_msg.content, "やあ");

        // まなみ（アシスタント）の発言は名前を持たず、content は本文そのもの。
        let assistant_msg = ChatMessage::assistant("やっほー");
        assert!(matches!(assistant_msg.role, Role::Assistant));
        assert_eq!(assistant_msg.content, "やっほー");
    }
}
