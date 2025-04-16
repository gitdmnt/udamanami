use std::{
    sync::{Arc, Mutex},
    time::Instant,
};

use dashmap::DashMap;

use regex::Regex;
use serenity::{
    all::Command,
    async_trait,
    builder::{CreateInteractionResponse, CreateInteractionResponseMessage},
    model::{
        application::Interaction,
        channel::Message,
        gateway::Ready,
        id::{ChannelId, GuildId, RoleId, UserId},
    },
    prelude::*,
};
use tracing::{error, info};

use calculator::EvalResult;
use commands::*;

pub mod commands;

pub mod ai;
pub mod calculator;
pub mod cclemon;
pub mod parser;

#[derive(Clone)]
pub struct UserData {
    pub room_pointer: ChannelId,
}

pub struct Bot {
    pub userdata: DashMap<UserId, UserData>,
    pub jail_process: Arc<DashMap<UserId, (usize, Instant)>>,
    pub jail_id: Arc<Mutex<usize>>,

    pub channel_ids: Vec<ChannelId>,
    pub guild_id: GuildId,
    pub erogaki_role_id: RoleId,
    pub jail_mark_role_id: RoleId,
    pub jail_main_role_id: RoleId,

    pub commit_hash: Option<String>,
    pub commit_date: Option<String>,

    pub variables: DashMap<String, EvalResult>,
    pub gemini: ai::GeminiAI,
}

impl Bot {
    pub fn get_user_room_pointer(&self, user_id: &UserId) -> ChannelId {
        self.userdata
            .entry(*user_id)
            .or_insert(UserData {
                room_pointer: self.channel_ids[0],
            })
            .clone()
            .room_pointer
    }

    pub fn change_room_pointer(
        &self,
        userid: &UserId,
        room_pointer: ChannelId,
    ) -> Result<(), anyhow::Error> {
        self.userdata
            .entry(*userid)
            .or_insert(UserData {
                room_pointer: self.channel_ids[0],
            })
            .room_pointer = room_pointer;
        Ok(())
    }
}

#[async_trait]
impl EventHandler for Bot {
    async fn message(&self, ctx: Context, msg: Message) {
        // Check if the message is from direct message
        match msg.guild_id {
            Some(_) => {
                guild_message(self, &ctx, &msg).await;
            }
            None => {
                direct_message(self, &ctx, &msg).await;
            }
        }
    }

    async fn ready(&self, ctx: Context, ready: Ready) {
        info!("{} is connected!", ready.user.name);

        let message_hello = match (self.commit_hash.clone(), self.commit_date.clone()) {
            (Some(commit_hash), Some(commit_date)) => {
                format!("おはようっ！ ||commit: {} ({})||", commit_hash, commit_date)
            }
            _ => "おはようっ！".to_owned(),
        };

        if let Err(why) = self.channel_ids[4].say(&ctx.http, message_hello).await {
            error!("Error sending message: {:?}", why);
        };

        // ローカルコマンドの登録
        let _ = self.guild_id.set_commands(&ctx.http, vec![]).await;

        // グローバルコマンドの登録
        let _ = Command::create_global_command(&ctx.http, help::register()).await;
        let _ = Command::create_global_command(&ctx.http, ping::register()).await;

        let _ = Command::create_global_command(&ctx.http, bf::register()).await;
        let _ = Command::create_global_command(&ctx.http, dice::register()).await;

        // roles のいずれかが付いているユーザーを恩赦
        let guild = self.guild_id;
        let roles = [self.jail_mark_role_id, self.jail_main_role_id];
        let members = guild.members(&ctx.http, None, None).await.unwrap();

        for member in members {
            if member.roles.iter().any(|role| roles.contains(role)) {
                let command_context = commands::CommandContext {
                    bot: self,
                    ctx: &ctx,
                    channel_id: &self.channel_ids[4],
                    author_id: &member.user.id,
                    command: "".to_owned(),
                };
                unjail::run(&command_context).await;
            }
        }
    }

    async fn interaction_create(&self, ctx: Context, interaction: Interaction) {
        if let Interaction::Command(command) = interaction {
            #[allow(unused_variables)]
            let command_context = CommandContext::new_from_command_interaction(
                self,
                &ctx,
                &command,
                &command.data.name,
            );
            let content = match command.data.name.as_str() {
                "help" => Some(help::run()),
                "ping" => Some(ping::run()),
                "bf" => Some(bf::run(&command.data.options())),
                "dice" => Some(dice::run(&command.data.options())),
                _ => Some("知らないコマンドだよ！".to_owned()),
            };
            if let Some(content) = content {
                let data = CreateInteractionResponseMessage::new().content(content);
                let builder = CreateInteractionResponse::Message(data);
                if let Err(why) = command.create_response(&ctx.http, builder).await {
                    error!("Error sending message: {:?}", why);
                }
            }
        }
    }
}

//direct message
async fn direct_message(bot: &Bot, ctx: &Context, msg: &Message) {
    if !has_privilege(bot, ctx, msg).await {
        return;
    }

    // if message is not command, forward to the room
    if !msg.content.starts_with('!') {
        // AIのためにメッセージを保存する
        bot.gemini.add_model_log(&msg.content);

        // 代筆先のチャンネルにメッセージを転送する
        let room_pointer = bot.get_user_room_pointer(&msg.author.id);
        if let Err(why) = room_pointer.say(&ctx.http, &msg.content).await {
            error!("Error sending message: {:?}", why);
        };
        return;
    }

    let command_context = CommandContext::new(bot, ctx, msg, msg.content[1..].to_string());

    // if message is command, handle command
    // handle command
    let command_name = &command_context.command_name()[..];

    match command_name {
        "channel" => channel::run(&command_context).await,
        "help" | "たすけて" | "助けて" => help::run_old(&command_context).await,
        "calc" => calc::run(&command_context).await,
        "var" => var::run(&command_context).await,
        "varbulk" => varbulk::run(&command_context).await,
        "calcsay" => calcsay::run(&command_context).await,

        // Unknown command
        _ => {
            let _ = &msg
                .channel_id
                .say(&ctx.http, "しらないコマンドだよ")
                .await
                .unwrap();
        }
    }
}

async fn guild_message(bot: &Bot, ctx: &Context, msg: &Message) {
    // 反応すべきメッセージかどうか確認
    if !has_privilege(bot, ctx, msg).await {
        return;
    }

    // AIのためにメッセージを保存する
    bot.gemini
        .add_user_log(msg.author.display_name(), &msg.content);

    // if message does not contains any command, ignore
    let command_pattern =
        Regex::new(r"(?ms)((?:まなみ(?:ちゃん)?(?:\s|、|は|って|の)?)|!)(.*)").unwrap();
    let (_prefix, input_string): (String, String) = match command_pattern.captures(&msg.content) {
        Some(caps) => (
            caps.get(1).unwrap().as_str().to_owned(),
            caps.get(2).unwrap().as_str().to_owned(),
        ),
        None => {
            return;
        }
    };

    let command_context = CommandContext::new(bot, ctx, msg, input_string.clone());

    // handle other command
    let command_name = &command_context.command_name()[..];

    match command_name {
        "help" | "たすけて" | "助けて" => {
            help::run_old(&command_context).await;
        }
        "isprime" => isprime::run(&command_context).await,
        "calc" => calc::run(&command_context).await,
        "var" => var::run(&command_context).await,
        "varbulk" => varbulk::run(&command_context).await,
        "cclemon" => commands::cclemon::run(&command_context).await,
        "jail" => jail::run_old(&command_context).await,
        "unjail" => unjail::run(&command_context).await,
        "clear" | "全部忘れて" => clear::run(&command_context).await,
        // Unknown command
        _ => {
            if msg.content.starts_with("!") {
                dice::run_old(&command_context).await
            } else if msg.channel_id.get() == bot.channel_ids[4].get() {
                // まなみが自由に応答するコーナー
                let content = bot.gemini.generate("gemini-2.0-flash-lite").await;
                let content = match content {
                    Ok(content) => content.replace("うだまなみ: ", ""),
                    Err(e) => {
                        format!("Error sending message: {:?}", e)
                    }
                };
                let _ = &msg.channel_id.say(&ctx.http, content).await;
            }
        }
    }
}

async fn has_privilege(bot: &Bot, ctx: &Context, msg: &Message) -> bool {
    if msg.author.bot {
        return false;
    }
    // ユーザーがこっちにきてはいけないに存在しない場合は無視
    if bot
        .guild_id
        .member(&ctx.http, &msg.author.id)
        .await
        .is_err()
    {
        return false;
    }
    true
}
