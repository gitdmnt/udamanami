use std::str::FromStr;

use anyhow::Context as _;
use dashmap::DashMap;
use serenity::async_trait;
use serenity::model::channel::Message;
use serenity::model::gateway::Ready;
use serenity::model::id::{ChannelId, UserId};
use serenity::prelude::*;
use serenity::utils::MessageBuilder;
use shuttle_runtime::SecretStore;
use tracing::{error, info};

const KOCHIKITE_GUILD_ID: u64 = 1066468273568362496;
const EROGAKI_ROLE_ID: u64 = 1066667753706102824;

struct Bot {
    userdata: DashMap<UserId, UserData>,
    channel_ids: Vec<ChannelId>,
}

struct UserData {
    room_pointer: ChannelId,
    is_erogaki: bool,
}

#[async_trait]
impl EventHandler for Bot {
    async fn message(&self, ctx: Context, msg: Message) {
        // if message is from bot, ignore
        if msg.author.bot {
            return;
        }

        // Check if the message is from direct message
        match msg.guild_id {
            Some(_) => {}
            None => {
                direct_message(self, &ctx, &msg).await;
            }
        }
    }

    async fn ready(&self, _: Context, ready: Ready) {
        info!("{} is connected!", ready.user.name);
    }
}

//direct message

async fn direct_message(bot: &Bot, ctx: &Context, msg: &Message) {
    // get user data
    let mut user = bot.userdata.entry(msg.author.id).or_insert(UserData {
        room_pointer: bot.channel_ids[0],
        is_erogaki: false,
    });
    // update user data
    let user = update_user(&ctx, &mut user, &msg.author.id).await.unwrap();

    // if message is not command, forward to the room
    if !&msg.content.starts_with("!") {
        if let Err(why) = user.room_pointer.say(&ctx.http, &msg.content).await {
            error!("Error sending message: {:?}", why);
        };
        return;
    }

    // if message is command, handle command
    // handle command
    let split_message = msg.content.split_whitespace().collect::<Vec<&str>>();
    let command_name = &split_message[0][1..]; // 先頭の "!" を削除
    let command_args = &split_message[1..];
    let dm = &msg.channel_id;

    match command_name {
        "ping" => ping(dm, ctx).await,
        "help" => help(dm, ctx).await,
        "erocheck" => erocheck(dm, ctx, user.is_erogaki).await,
        "channel" => channel(dm, ctx, command_args, &bot.channel_ids, user).await,

        // Unknown command
        _ => {
            dm.say(&ctx.http, "しらないコマンドだよ").await.unwrap();
        }
    }
}

async fn update_user<'a>(
    ctx: &Context,
    user: &'a mut UserData,
    author: &UserId,
) -> Result<&'a mut UserData, anyhow::Error> {
    // erogaki role check
    user.is_erogaki = author
        .to_user(&ctx.http)
        .await?
        .has_role(&ctx.http, KOCHIKITE_GUILD_ID, EROGAKI_ROLE_ID)
        .await?;
    Ok(user)
}

// commands
// ping command
async fn ping(reply: &ChannelId, ctx: &Context) {
    reply.say(&ctx.http, "pong").await.unwrap();
}

async fn help(reply: &ChannelId, ctx: &Context) {
    reply
        .say(
            &ctx.http,
            r"```
!ping       pong!
!help       このヘルプを表示するよ
!erocheck   あなたがエロガキかどうかを判定するよ
!channel    代筆先のチャンネルについてだよ
```",
        )
        .await
        .unwrap();
}

// erogaki status check
async fn erocheck(reply: &ChannelId, ctx: &Context, is_erogaki: bool) {
    match is_erogaki {
        true => {
            reply.say(&ctx.http, "エロガキ！！！！").await.unwrap();
        }
        false => {
            reply.say(&ctx.http, "エロガキじゃないよ").await.unwrap();
        }
    }
}

// change channel
async fn channel(
    reply: &ChannelId,
    ctx: &Context,
    args: &[&str],
    channels: &Vec<ChannelId>,
    user: &mut UserData,
) {
    // 引数なしの場合はチャンネル一覧を表示
    if args.len() == 0 {
        let mut res = MessageBuilder::new();
        res.push("今は")
            .channel(user.room_pointer)
            .push("で代筆してるよ\n")
            .push("```チャンネル一覧だよ\n");
        for (i, ch) in channels.iter().enumerate() {
            res.push(format!("{i:>2}\t"))
                .push(ch.name(&ctx.http).await.unwrap())
                .push("\n");
        }
        let res = res.push("```").push("使い方: `!channel <ID>`").build();

        reply.say(&ctx.http, &res).await.unwrap();
        return;
    }

    // それ以外の場合は指定されたチャンネルに切り替え
    let next_pointer = channels[args[0].parse::<usize>().unwrap()];
    user.room_pointer = next_pointer;
    reply
        .say(
            &ctx.http,
            MessageBuilder::new()
                .push("送信先を")
                .channel(next_pointer)
                .push("に設定したよ")
                .build(),
        )
        .await
        .unwrap();
}

#[shuttle_runtime::main]
async fn serenity(
    #[shuttle_runtime::Secrets] secrets: SecretStore,
) -> shuttle_serenity::ShuttleSerenity {
    // Get the discord token set in `Secrets.toml`
    let token = secrets
        .get("DISCORD_TOKEN")
        .context("'DISCORD_TOKEN' was not found")?;

    // Set gateway intents, which decides what events the bot will be notified about
    let intents = GatewayIntents::GUILD_MESSAGES
        | GatewayIntents::MESSAGE_CONTENT
        | GatewayIntents::DIRECT_MESSAGES;

    let userdata = DashMap::new();
    let channel_ids = vec![
        secrets.get("FREETALK1_ROOM_ID"),
        secrets.get("FREETALK2_ROOM_ID"),
        secrets.get("MADSISTERS_ROOM_ID"),
        secrets.get("SHYBOYS_ROOM_ID"),
        secrets.get("DEBUG_ROOM_ID"),
    ]
    .into_iter()
    .map(|id| ChannelId::from_str(&id.unwrap()).unwrap())
    .collect();

    let client = Client::builder(&token, intents)
        .event_handler(Bot {
            userdata,
            channel_ids,
        })
        .await
        .expect("Err creating client");

    Ok(client.into())
}
