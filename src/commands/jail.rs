use crate::commands::unjail;
use crate::commands::{CommandContext, ManamiSlashCommand};

use std::time::{Duration, Instant};

use serenity::{
    builder::{CreateCommand, CreateCommandOption},
    model::application::{CommandOptionType, ResolvedOption, ResolvedValue},
    prelude::*,
};
use tokio::{spawn, time::sleep};

pub const SLASH_JAIL_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "jail",
    usage: "/jail <user> [term]",
    description: "不届き者を収監して 見せます・袋とじ・管理 以外のカテゴリで喋れなくするよ！",
    register,
    run: |options, ctx| Box::pin(async move { run(options, &ctx).await }),
    is_local_command: true,
};

const JAIL_TERM_DEFAULT: Duration = Duration::from_secs(15);

pub fn register() -> CreateCommand {
    CreateCommand::new("jail")
        .description("指定した人を指定した時間だけ収監するよ")
        .add_option(
            CreateCommandOption::new(CommandOptionType::User, "user", "収監する人").required(true),
        )
        .add_option(
            CreateCommandOption::new(CommandOptionType::Integer, "term", "刑期（秒）")
                .required(false)
                .min_int_value(0)
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
    let reply = ctx.channel_id;
    let ctx_clone = ctx.ctx.clone();
    let process = bot.jail_process.clone();
    spawn(async move {
        sleep(jailterm).await;
        unjail::unjail_and_notify(
            reply,
            &ctx_clone,
            user,
            &guild,
            &roles,
            Some(newid),
            &process,
        )
        .await;
    });
    result.unwrap()
}
