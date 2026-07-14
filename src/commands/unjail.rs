use std::{sync::Arc, time::Instant};

use dashmap::DashMap;

use serenity::{
    builder::{CreateCommand, CreateCommandOption},
    model::application::{CommandOptionType, ResolvedOption, ResolvedValue},
    model::id::{ChannelId, GuildId, RoleId, UserId},
    prelude::*,
};

use crate::commands::{CommandContext, ManamiSlashCommand};

pub const SLASH_UNJAIL_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "unjail",
    usage: "/unjail <user>",
    description: "収監を解除するよ！",
    register,
    run: |options, ctx| Box::pin(async move { run_body(options, &ctx).await }),
    is_local_command: true,
};

pub fn register() -> CreateCommand {
    CreateCommand::new("unjail")
        .description("収監を解除するよ！")
        .add_option(
            CreateCommandOption::new(CommandOptionType::User, "user", "釈放する人").required(true),
        )
}

async fn run_body(options: Vec<ResolvedOption<'_>>, ctx: &CommandContext<'_>) -> String {
    let user = options
        .iter()
        .fold(None, |user, option| match (option.name, &option.value) {
            ("user", ResolvedValue::User(user, _)) => Some(user.id),
            _ => user,
        });
    let Some(user) = user else {
        return "誰？".to_owned();
    };

    let bot = ctx.bot;
    unjail(
        ctx.ctx,
        user,
        &bot.guild_id,
        &[bot.jail_mark_role_id, bot.jail_main_role_id],
        None,
        &bot.jail_process,
    )
    .await
    .unwrap_or_else(|| "その人は収監されてないよ".to_owned())
}

/// 釈放処理を実行する。実際に何か行った場合はユーザーへ通知するメッセージを、
/// 何もしなかった場合は `None` を返す（呼び出し側が送信する）。
pub async fn unjail(
    ctx: &Context,
    user: UserId,
    guild: &GuildId,
    roles: &[RoleId],
    jail_id: Option<usize>,
    jail_process: &Arc<DashMap<UserId, (usize, Instant)>>,
) -> Option<String> {
    let member = guild.member(&ctx.http, user).await.unwrap();

    // 釈放予定表を確認
    if let Some((id, _)) = jail_process.get(&user).map(|r| *r.value()) {
        if let Some(jail_id) = jail_id {
            // 釈放するidが指定されている場合
            if jail_id == id {
                //当該idのみ釈放
                jail_process.remove(&user);
            } else {
                return None; // 釈放しない
            }
        } else {
            // 無条件釈放なら、釈放予定表から削除
            jail_process.remove(&user);
        }
    }

    if !member.roles.iter().any(|role| roles.contains(role)) {
        return None;
    }

    if member.remove_roles(&ctx.http, roles).await.is_err() {
        return Some("釈放に失敗したよ".to_owned());
    }

    Some(format!("{}を釈放したよ", user.mention()))
}

/// `unjail` を実行し、通知メッセージがあれば `reply` チャンネルに送信する。
pub async fn unjail_and_notify(
    reply: ChannelId,
    ctx: &Context,
    user: UserId,
    guild: &GuildId,
    roles: &[RoleId],
    jail_id: Option<usize>,
    jail_process: &Arc<DashMap<UserId, (usize, Instant)>>,
) {
    if let Some(content) = unjail(ctx, user, guild, roles, jail_id, jail_process).await {
        let _ = reply.say(&ctx.http, content).await;
    }
}
