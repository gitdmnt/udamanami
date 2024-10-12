use anyhow::Context as _;
use serenity::async_trait;
use serenity::model::channel::Message;
use serenity::model::gateway::Ready;
use serenity::model::id::{ChannelId, UserId};
use serenity::prelude::*;
use shuttle_runtime::SecretStore;
use std::collections::HashMap;
use tracing::{error, info};

const KOCHIKITE_GUILD_ID: u64 = 1066468273568362496;
const EROGAKI_ROLE_ID: u64 = 1066667753706102824;

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
        room_pointer: ChannelId::from(1245427348698824784),
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
    let command = vec[0];
    let args = &vec[1..];

    match command {
        "erocheck" => match user.is_erogaki {
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
        },
        _ => {
            msg.channel_id
                .say(&ctx.http, "しらないコマンドだよ")
                .await
                .unwrap();
        }
    }
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
