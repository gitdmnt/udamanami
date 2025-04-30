use core::time::Duration;

use serenity::{all::Message, builder::GetMessages};

const COMMAND_NAME: &str = "imakita";

pub const SLASH_AUTO_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: COMMAND_NAME,
    usage: "/imakita",
    description: "今北産業",
    register,
    run: |option, bot| {
        let opts = parse_options(option, bot);
        Box::pin(async move { run_body(opts, bot).await })
    },
    is_local_command: true,
};

pub fn register() -> serenity::builder::CreateCommand {
    serenity::builder::CreateCommand::new("imakita").description("今北産業")
}

pub async fn run(
    channel_id: &serenity::model::id::ChannelId,
    ctx: &serenity::client::Context,
    api_key: &str,
) -> String {
    // fetch logs
    let mut log: Vec<Message> = Vec::new();
    'outer: loop {
        let builder = GetMessages::new().limit(100);
        if !log.is_empty() {
            let _ = builder.before(log.last().unwrap().id);
        }
        let mut messages = channel_id
            .messages(&ctx.http, GetMessages::new().limit(100))
            .await
            .unwrap();
        messages.reverse();

        let mut timestamp = messages.first().unwrap().timestamp;
        for message in messages {
            // 1時間以上間隔が開いたら打ち切り
            if timestamp.to_utc() - message.timestamp.to_utc() > Duration::from_secs(3600) {
                break 'outer;
            }
            timestamp = message.timestamp;
            log.push(message);
        }
    }

    // ログを整形
    let mut log_str = vec![];
    for message in log.iter() {
        // ごめんけどハードコードするね
        // Context::cache.current_user_id()
        let name = if message.author.id == 1098568941208613015 {
            "model".to_owned()
        } else {
            message
                .author_nick(&ctx.http)
                .await
                .unwrap_or_else(|| message.author.name.clone())
        };
        log_str.push((name, message.content.as_str()));
    }

    // summerize by gemini
    let gemini = crate::ai::GeminiAI::new(api_key);
    gemini.add_log_bulk(log_str);

    let content = gemini.generate().await.unwrap();

    unimplemented!();
}
