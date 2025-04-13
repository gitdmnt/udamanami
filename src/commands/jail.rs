use crate::calculator;
use crate::commands::unjail;
use crate::commands::CommandContext;
use crate::Bot;

use std::time::{Duration, Instant};

use serenity::{
    builder::{CreateCommand, CreateCommandOption},
    model::application::{CommandOptionType, ResolvedOption, ResolvedValue},
    model::id::{ChannelId, UserId},
    prelude::*,
    utils::parse_user_mention,
};
use tokio::{spawn, time::sleep};

const JAIL_TERM_MAX: Duration = Duration::from_secs(3600);
const JAIL_TERM_DEFAULT: Duration = Duration::from_secs(15);

// slash command
pub fn register() -> CreateCommand {
    CreateCommand::new("jail")
        .description("指定した人を指定した時間だけ収監するよ")
        .add_option(
            CreateCommandOption::new(CommandOptionType::User, "user", "収監する人").required(true),
        )
        .add_option(
            CreateCommandOption::new(CommandOptionType::Integer, "term", "刑期（秒）")
                .required(false),
        )
}

pub async fn run(options: &[ResolvedOption<'_>], ctx: &CommandContext<'_>) -> String {
    let user = match options[0].value {
        ResolvedValue::User(user, _) => user.id,
        _ => {
            return "誰？".to_owned();
        }
    };
    let jailterm = match options[1].value {
        ResolvedValue::Integer(term) => {
            if term > JAIL_TERM_MAX.as_secs() as i64 {
                JAIL_TERM_MAX
            } else {
                Duration::from_secs(term as u64)
            }
        }
        _ => JAIL_TERM_DEFAULT,
    };
    let bot = ctx.bot;
    let guild = bot.guild_id;
    let roles = vec![bot.jail_mark_role_id, bot.jail_main_role_id];
    let member = guild.member(&ctx.cache_http(), user).await.unwrap();
    if member.add_roles(&ctx.cache_http(), &roles).await.is_err() {
        return "収監に失敗したよ".to_owned();
    }
    let jail_term_end = Instant::now() + jailterm;
    let content = if let Some((_, end)) = bot.jail_process.get(&user).map(|r| *r.value()) {
        if end > jail_term_end {
            let content = format!(
                "{}はすでに収監中だよ（残り刑期：{}秒）",
                user.mention(),
                end.duration_since(Instant::now()).as_secs()
            );
            Err(content)
        } else {
            let content = format!(
                "{}を再収監したよ（残り刑期：{}秒 → {}秒）",
                user.mention(),
                end.duration_since(Instant::now()).as_secs(),
                jailterm.as_secs()
            );
            Ok(content)
        }
    } else {
        let content = format!(
            "{}を収監したよ（刑期：{}秒）",
            user.mention(),
            jailterm.as_secs()
        );
        Ok(content)
    };

    let Ok(content) = content else {
        return content.unwrap_err();
    };

    let Some(newid) = bot.jail_id.lock().map_or(None, |mut oldid| {
        *oldid += 1;
        Some(*oldid)
    }) else {
        return "再収監に失敗したよ".to_owned();
    };

    let reply = *ctx.channel_id;
    let ctx = ctx.ctx.clone();
    let roles = roles.to_vec();
    bot.jail_process.insert(user, (newid, jail_term_end));
    let process = bot.jail_process.clone();
    spawn(async move {
        sleep(jailterm).await;
        unjail::unjail(&reply, &ctx, &user, &guild, &roles, Some(newid), &process).await;
        drop(process);
    });
    content
}
pub async fn run_old(ctx: &CommandContext<'_>) {
    let reply = ctx.channel_id;
    let args = &ctx.args()[..];
    let bot = ctx.bot;
    let ctx = ctx.ctx;

    let (user, jailterm) = match args {
        [user] => {
            let Some(user) = parse_user_mention(user) else {
                reply.say(&ctx.http, "誰？").await.unwrap();
                return;
            };
            (user, JAIL_TERM_DEFAULT)
        }
        [user, args @ ..] => {
            let Some(user) = parse_user_mention(user) else {
                reply.say(&ctx.http, "誰？").await.unwrap();
                return;
            };

            let expression = args.join(" ");

            let Ok(jailtermsec) = calculator::eval_from_str(&expression, &bot.variables) else {
                reply.say(&ctx.http, "刑期がおかしいよ").await.unwrap();
                return;
            };
            let Some(jailtermsec) = calculator::val_as_int(&jailtermsec) else {
                reply.say(&ctx.http, "刑期がおかしいよ").await.unwrap();
                return;
            };
            let Ok(jailtermsec) = u64::try_from(jailtermsec) else {
                reply.say(&ctx.http, "刑期が負だよ").await.unwrap();
                return;
            };

            let jailterm = if jailtermsec > JAIL_TERM_MAX.as_secs() {
                reply
                    .say(
                        &ctx.http,
                        format!(
                            "刑期が長すぎるから切り詰めたよ（最長{}秒）",
                            JAIL_TERM_MAX.as_secs()
                        ),
                    )
                    .await
                    .unwrap();
                JAIL_TERM_MAX
            } else {
                Duration::from_secs(jailtermsec)
            };
            (user, jailterm)
        }
        _ => {
            reply
                .say(&ctx.http, "使い方: `!jail <user> [刑期（秒）]`")
                .await
                .unwrap();
            return;
        }
    };
    jail(reply, ctx, &user, jailterm, bot).await;
}

async fn jail(reply: &ChannelId, ctx: &Context, user: &UserId, jailterm: Duration, bot: &Bot) {
    let guild = bot.guild_id;
    let roles = vec![bot.jail_mark_role_id, bot.jail_main_role_id];

    let member = guild.member(&ctx.http, user).await.unwrap();
    if member.add_roles(&ctx.http, &roles).await.is_err() {
        reply.say(&ctx.http, "収監に失敗したよ").await.unwrap();
        return;
    }

    let jail_term_end = Instant::now() + jailterm;
    if let Some((_, end)) = bot.jail_process.get(user).map(|r| *r.value()) {
        if end > jail_term_end {
            let content = format!(
                "{}はすでに収監中だよ（残り刑期：{}秒）",
                user.mention(),
                end.duration_since(Instant::now()).as_secs()
            );
            reply.say(&ctx.http, content).await.unwrap();
            return; // 既に収監中
        } else {
            let content = format!(
                "{}を再収監したよ（残り刑期：{}秒 → {}秒）",
                user.mention(),
                end.duration_since(Instant::now()).as_secs(),
                jailterm.as_secs()
            );

            reply.say(&ctx.http, content).await.unwrap();
        }
    } else {
        let content = format!(
            "{}を収監したよ（刑期：{}秒）",
            user.mention(),
            jailterm.as_secs()
        );

        reply.say(&ctx.http, content).await.unwrap();
    }

    let Some(newid) = bot.jail_id.lock().map_or(None, |mut oldid| {
        *oldid += 1;
        Some(*oldid)
    }) else {
        reply.say(&ctx.http, "再収監に失敗したよ").await.unwrap();
        return;
    };

    let reply = *reply;
    let ctx = ctx.clone();
    let user = *user;
    let roles = roles.to_vec();
    bot.jail_process.insert(user, (newid, jail_term_end));
    let process = bot.jail_process.clone();
    spawn(async move {
        sleep(jailterm).await;
        unjail::unjail(&reply, &ctx, &user, &guild, &roles, Some(newid), &process).await;
        drop(process);
    });
}
