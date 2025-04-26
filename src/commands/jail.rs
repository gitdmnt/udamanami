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

use crate::commands::ManamiPrefixCommand;
pub const PREFIX_JAIL_COMMAND: ManamiPrefixCommand = ManamiPrefixCommand {
    name: "jail",
    alias: &[],
    usage: "!jail <n>",
    description: "不届き者を収監して 見せます・袋とじ・管理 以外のカテゴリで喋れなくするよ！",
    run: |ctx| Box::pin(run_old(ctx)),
    is_dm_command: false,
    is_guild_command: true,
};

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
                .required(false)
                .max_int_value(0)
                .max_int_value(3600),
        )
}

pub async fn run(options: Vec<ResolvedOption<'_>>, ctx: &CommandContext<'_>) -> String {
    // 引数からユーザと刑期を取得
    let (user, jailterm) = options.iter().fold(
        (None, JAIL_TERM_DEFAULT),
        |(user, jailterm), option| match (option.name, &option.value) {
            ("user", ResolvedValue::User(user, _)) => (Some(*user), jailterm),
            ("term", ResolvedValue::Integer(term)) => (user, Duration::from_secs(*term as u64)),
            _ => (user, jailterm),
        },
    );
    let user = user.unwrap().id;
    let bot = ctx.bot;
    let guild = bot.guild_id;
    let roles = vec![bot.jail_mark_role_id, bot.jail_main_role_id];

    // 指定ユーザーにロールを追加
    let member = guild.member(&ctx.cache_http(), user).await.unwrap();
    if member.add_roles(&ctx.cache_http(), &roles).await.is_err() {
        return "収監に失敗したよ".to_owned();
    }

    // 既存の収監状況確認とメッセージ生成
    let now = Instant::now();
    let jail_term_end = now + jailterm;
    let result = if let Some((_, end)) = bot.jail_process.get(&user).map(|r| *r.value()) {
        if end > jail_term_end {
            Err(format!(
                "{}はすでに収監中だよ（残り刑期：{}秒）",
                user.mention(),
                end.duration_since(now).as_secs()
            ))
        } else {
            Ok(format!(
                "{}を再収監したよ（残り刑期：{}秒 → {}秒）",
                user.mention(),
                end.duration_since(now).as_secs(),
                jailterm.as_secs()
            ))
        }
    } else {
        Ok(format!(
            "{}を収監したよ（刑期：{}秒）",
            user.mention(),
            jailterm.as_secs()
        ))
    };
    if let Err(result) = result {
        return result;
    };

    // 新しい識別子を生成して収監情報を更新
    let newid = if let Ok(mut guard) = bot.jail_id.lock() {
        *guard += 1;
        *guard
    } else {
        return "再収監に失敗したよ".to_owned();
    };
    bot.jail_process.insert(user, (newid, jail_term_end));

    // 非同期で刑期後の解除処理をスケジュール
    let reply = *ctx.channel_id;
    let ctx_clone = ctx.ctx.clone();
    let process = bot.jail_process.clone();
    spawn(async move {
        sleep(jailterm).await;
        unjail::unjail(
            &reply,
            &ctx_clone,
            &user,
            &guild,
            &roles,
            Some(newid),
            &process,
        )
        .await;
    });
    result.unwrap()
}

pub async fn run_old(ctx: CommandContext<'_>) {
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
