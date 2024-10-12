use anyhow::Context as _;
use serenity::async_trait;
use serenity::model::channel::Message;
use serenity::model::gateway::Ready;
use serenity::model::id::{ChannelId, UserId};
use serenity::prelude::*;
use serenity::utils::MessageBuilder;
use shuttle_runtime::SecretStore;
use std::collections::HashMap;
use tracing::{error, info};

const KOCHIKITE_GUILD_ID: u64 = 1066468273568362496;
const EROGAKI_ROLE_ID: u64 = 1066667753706102824;

const DEBUG_ROOM_ID: u64 = 1245427348698824784;
const FREETALK1_ROOM_ID: u64 = 1066470646827204719;
const FREETALK2_ROOM_ID: u64 = 1066483929848217680;
const MADSISTERS_ROOM_ID: u64 = 1066639830735396924;
const SHYBOYS_ROOM_ID: u64 = 1067059988276727859;

struct Bot {
    userdata: Mutex<HashMap<UserId, UserData>>,
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

async fn direct_message(bot: &Bot, ctx: &Context, msg: &Message) {
    let author = msg.author.id;

    // Get the user data
    let mut user = bot.userdata.lock().await;
    let user = user.entry(author).or_insert(UserData {
        room_pointer: ChannelId::from(FREETALK1_ROOM_ID),
        is_erogaki: false,
    });
    // erogaki role check
    user.is_erogaki = author
        .to_user(&ctx.http)
        .await
        .unwrap()
        .has_role(&ctx.http, KOCHIKITE_GUILD_ID, EROGAKI_ROLE_ID)
        .await
        .unwrap();

    // command or not
    match msg.content.chars().nth(0) {
        Some('!') => {
            // Handle command
            handle_command(ctx, msg, user).await;
        }
        _ => {
            // Forward the message to the room
            if let Err(why) = user.room_pointer.say(&ctx.http, &msg.content).await {
                error!("Error sending message: {:?}", why);
            }
        }
    }
}

async fn handle_command(ctx: &Context, msg: &Message, user: &mut UserData) {
    let vec = msg.content.split_whitespace().collect::<Vec<&str>>();
    let command = &vec[0][1..];
    let args = &vec[1..];

    match command {
        "ping" => ping(ctx, msg, user).await,
        "help" => help(ctx, msg).await,
        "erocheck" => erocheck(ctx, msg, user).await,
        "channel" => channel(ctx, msg, args, user).await,

        // Unknown command
        _ => {
            msg.channel_id
                .say(&ctx.http, "しらないコマンドだよ")
                .await
                .unwrap();
        }
    }
}

// commands

// ping command
async fn ping(ctx: &Context, msg: &Message, _: &mut UserData) {
    msg.channel_id.say(&ctx.http, "pong").await.unwrap();
}

async fn help(ctx: &Context, msg: &Message) {
    msg.channel_id
        .say(
            &ctx.http,
            MessageBuilder::new()
                .push("```")
                .push("!ping\tpong!\n")
                .push("!help\tこのヘルプを表示するよ\n")
                .push("!erocheck\tあなたがエロガキかどうかを判定するよ\n")
                .push("!channel\t代筆先のチャンネルについてだよ\n")
                .push("```")
                .build(),
        )
        .await
        .unwrap();
}

// erogaki status check
async fn erocheck(ctx: &Context, msg: &Message, user: &mut UserData) {
    match user.is_erogaki {
        true => {
            msg.channel_id
                .say(&ctx.http, "エロガキ！！！！")
                .await
                .unwrap();
        }
        false => {
            msg.channel_id
                .say(&ctx.http, "エロガキじゃないよ")
                .await
                .unwrap();
        }
    }
}

// change channel
async fn channel(ctx: &Context, msg: &Message, args: &[&str], user: &mut UserData) {
    // Get the channel list from the guild
    let channels = [
        ChannelId::from(FREETALK1_ROOM_ID),
        ChannelId::from(FREETALK2_ROOM_ID),
        ChannelId::from(MADSISTERS_ROOM_ID),
        ChannelId::from(SHYBOYS_ROOM_ID),
        ChannelId::from(DEBUG_ROOM_ID),
    ];
    if args.len() == 0 {
        let mut response = MessageBuilder::new();
        response
            .push("今は")
            .channel(user.room_pointer)
            .push("で代筆してるよ\n")
            .push("```チャンネル一覧だよ\n");
        for (i, channel) in channels.iter().enumerate() {
            response
                .push(format!("{i:>2}\t"))
                .push(channel.name(&ctx).await.unwrap())
                .push("\n");
        }
        let response = response.push("```").push("使い方: `!channel <ID>`").build();

        msg.channel_id.say(&ctx.http, &response).await.unwrap();
        return;
    }

    let channel = channels[args[0].parse::<usize>().unwrap()];
    user.room_pointer = channel;
    msg.channel_id
        .say(
            &ctx.http,
            MessageBuilder::new()
                .push("送信先を")
                .channel(channel)
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

    let userdata = Mutex::from(HashMap::new());

    let client = Client::builder(&token, intents)
        .event_handler(Bot { userdata })
        .await
        .expect("Err creating client");

    Ok(client.into())
}
