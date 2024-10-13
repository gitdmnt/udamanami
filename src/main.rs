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
            Some(_) => {
                guild_message(self, &ctx, &msg).await;
            }
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
        "channel" => channel(dm, ctx, command_args, &bot.channel_ids, user).await,
        "erocheck" => erocheck(dm, ctx, user.is_erogaki).await,
        "help" => help(dm, ctx).await,
        "ping" => ping(dm, ctx).await,

        // Unknown command
        _ => {
            dm.say(&ctx.http, "しらないコマンドだよ").await.unwrap();
        }
    }
}

async fn guild_message(bot: &Bot, ctx: &Context, msg: &Message) {
    // if message is not command, ignore
    if !&msg.content.starts_with("!") {
        return;
    }

    // get user data
    let mut user = bot.userdata.entry(msg.author.id).or_insert(UserData {
        room_pointer: bot.channel_ids[0],
        is_erogaki: false,
    });
    // update user data
    let user = update_user(&ctx, &mut user, &msg.author.id).await.unwrap();

    // handle command
    let split_message = msg.content.split_whitespace().collect::<Vec<&str>>();
    let command_name = &split_message[0][1..]; // 先頭の "!" を削除
    let command_args = &split_message[1..];
    let reply_channel = &msg.channel_id;

    // dice command
    let re = regex::Regex::new(r"(\d*)(d|D)(\d+)").unwrap();
    if re.is_match(command_name) {
        dice(reply_channel, ctx, command_name, command_args).await;
        return;
    }

    match command_name {
        "help" => help(reply_channel, ctx).await,
        // Unknown command
        _ => {
            reply_channel
                .say(&ctx.http, "しらないコマンドだよ")
                .await
                .unwrap();
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

// help command
async fn help(reply: &ChannelId, ctx: &Context) {
    reply
        .say(
            &ctx.http,
            r"ダイレクトメッセージで使えるコマンドは次の通りだよ
```
!channel    代筆先のチャンネルについてだよ
!erocheck   あなたがエロガキかどうかを判定するよ
!help       このヘルプを表示するよ
!ping       pong!
```
グループチャットで使えるコマンドは次の通りだよ
```
![n]d<m>    m面ダイスをn回振るよ
!help       このヘルプを表示するよ
```",
        )
        .await
        .unwrap();
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
    let selector = match args[0].parse::<usize>() {
        Ok(i) => {
            if i >= channels.len() {
                reply
                    .say(&ctx.http, "しらないチャンネルだよ")
                    .await
                    .unwrap();
                return;
            } else {
                i
            }
        }
        Err(_) => {
            reply.say(&ctx.http, "IDは数字で指定してね").await.unwrap();
            return;
        }
    };

    let next_pointer = channels[selector];
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

// dice command
async fn dice(reply: &ChannelId, ctx: &Context, command_name: &str, command_args: &[&str]) {
    let re = regex::Regex::new(r"(\d*)(d|D)(\d+)").unwrap();
    let caps = re.captures(command_name).unwrap();
    let num: u128 = caps.get(1).map_or(1, |m| m.as_str().parse().unwrap());
    let dice: u128 = caps.get(3).map_or(6, |m| m.as_str().parse().unwrap());
    let mut sum = 0;
    let mut res = MessageBuilder::new();
    let mut vec = vec![];

    for _ in 0..num {
        let r = rand::random::<u128>() % dice + 1;
        vec.push(r.to_string());
        sum += r;
    }
    res.push(format!("{}D{} -> {}", num, dice, sum));
    res.push(format!("({})", vec.join(", ")));

    if command_args.len() == 0 {
        reply.say(&ctx.http, &res.build()).await.unwrap();
        return;
    } else if command_args.len() != 2 {
        reply.say(&ctx.http, &res.build()).await.unwrap();
        reply
            .say(&ctx.http, "使い方: `!<num>d<dice> [<operator> <num>]`")
            .await
            .unwrap();
        return;
    }

    let operator = command_args[0];
    let operand = command_args[1].parse::<u128>().unwrap();
    let is_ok = match operator {
        "<=" => sum <= operand,
        "<" => sum < operand,
        "=" => sum == operand,
        "==" => sum == operand,
        "!=" => sum != operand,
        ">" => sum > operand,
        ">=" => sum >= operand,
        _ => {
            reply.say(&ctx.http, &res.build()).await.unwrap();
            reply
                .say(&ctx.http, "`<operator> = <|<=|=|==|!=|>|>=`だよ")
                .await
                .unwrap();
            return;
        }
    };
    let is_ok = if is_ok { "OK" } else { "NG" };
    res.push(format!(" {} {} -> {}", operator, operand, is_ok));

    reply.say(&ctx.http, &res.build()).await.unwrap();
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

// ping command
async fn ping(reply: &ChannelId, ctx: &Context) {
    reply.say(&ctx.http, "pong").await.unwrap();
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
