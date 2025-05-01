use core::time::Duration;

use serenity::{all::Message, builder::GetMessages};

use super::CommandContext;

pub fn register() -> serenity::builder::CreateCommand {
    serenity::builder::CreateCommand::new("imakita").description("今北産業")
}

pub async fn run(ctx: &CommandContext<'_>) -> String {
    // fetch logs
    let mut log: Vec<Message> = Vec::new();
    'outer: loop {
        let builder = GetMessages::new().limit(100);
        if !log.is_empty() {
            let _ = builder.before(log.last().unwrap().id);
        }
        let mut messages = ctx
            .channel_id
            .messages(ctx.cache_http(), GetMessages::new().limit(100))
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
        let name = if message.author.id == 1098568941208613015 {
            "model".to_owned()
        } else {
            message
                .author_nick(&ctx.cache_http())
                .await
                .unwrap_or_else(|| message.author.name.clone())
        };
        log_str.push((name, message.content.as_str()));
    }

    // summerize by gemini
    let gemini = &ctx.bot.gemini;
    gemini.add_log_bulk(log_str);

    gemini.generate().await.unwrap()
}
