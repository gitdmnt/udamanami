use crate::commands::CommandContext;
use std::{sync::Arc, time::Instant};

use dashmap::DashMap;

use serenity::{
    all::ResolvedOption,
    model::id::{ChannelId, GuildId, RoleId, UserId},
    prelude::*,
    utils::parse_user_mention,
};

use super::ManamiPrefixCommand;

pub struct PrefixCommand;

impl ManamiPrefixCommand for PrefixCommand {
    fn name(&self) -> &'static [&'static str] {
        &["unjail"]
    }

    fn usage(&self) -> &'static str {
        "!unjail <user>"
    }

    fn description(&self) -> &'static str {
        "収監を解除するよ！"
    }

    async fn run(&self, ctx: &CommandContext<'_>, _: &[ResolvedOption<'_>]) {
        run(ctx).await
    }

    fn is_dm_command(&self) -> bool {
        false
    }

    fn is_guild_command(&self) -> bool {
        true
    }
}

pub async fn run(ctx: &CommandContext<'_>) {
    let reply = ctx.channel_id;
    let args = &ctx.args()[..];
    let bot = ctx.bot;
    let ctx = ctx.ctx;
    let [user] = args else {
        reply
            .say(&ctx.http, "使い方: `!unjail <user>`")
            .await
            .unwrap();
        return;
    };
    let Some(user) = parse_user_mention(user) else {
        reply.say(&ctx.http, "誰？").await.unwrap();
        return;
    };

    unjail(
        reply,
        ctx,
        &user,
        &bot.guild_id,
        &[bot.jail_mark_role_id, bot.jail_main_role_id],
        None,
        &bot.jail_process,
    )
    .await;
}

pub async fn unjail(
    reply: &ChannelId,
    ctx: &Context,
    user: &UserId,
    guild: &GuildId,
    roles: &[RoleId],
    jail_id: Option<usize>,
    jail_process: &Arc<DashMap<UserId, (usize, Instant)>>,
) {
    let member = guild.member(&ctx.http, user).await.unwrap();

    // 釈放予定表を確認
    if let Some((id, _)) = jail_process.get(user).map(|r| *r.value()) {
        if let Some(jail_id) = jail_id {
            // 釈放するidが指定されている場合
            if jail_id == id {
                //当該idのみ釈放
                jail_process.remove(user);
            } else {
                return; // 釈放しない
            }
        } else {
            // 無条件釈放なら、釈放予定表から削除
            jail_process.remove(user);
        }
    }

    if !member.roles.iter().any(|role| roles.contains(role)) {
        return;
    }

    if member.remove_roles(&ctx.http, roles).await.is_err() {
        reply.say(&ctx.http, "釈放に失敗したよ").await.unwrap();
        return;
    }

    let content = format!("{}を釈放したよ", user.mention());
    reply.say(&ctx.http, content).await.unwrap();
}
