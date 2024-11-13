use std::str::FromStr;
use std::sync::Mutex;
use std::time::Duration;
use std::{collections::VecDeque, convert::Into};

use anyhow::Context as _;
use dashmap::DashMap;
use nom::{
    error::{Error, ErrorKind},
    Finish as _,
};
use regex::Regex;
use serenity::{
    async_trait,
    model::{
        channel::Message,
        gateway::Ready,
        id::{ChannelId, GuildId, RoleId, UserId},
    },
    prelude::*,
    utils::{parse_user_mention, MessageBuilder},
};
use shuttle_runtime::SecretStore;
use tracing::{error, info};

use std::cmp::min;
use tokio::{spawn, time::sleep};

mod calculator;
mod cclemon;
mod parser;

use calculator::EvalResult;
use udamanami::ai;

const KOCHIKITE_GUILD_ID: u64 = 1066468273568362496;
const EROGAKI_ROLE_ID: u64 = 1066667753706102824;
const JAIL_MARK_ROLE_ID: u64 = 1305240882923962451;
const JAIL_MAIN_ROLE_ID: u64 = 1305228980697305119;

struct Bot {
    userdata: DashMap<UserId, UserData>,
    channel_ids: Vec<ChannelId>,
    guild_id: GuildId,
    erogaki_role_id: RoleId,
    jail_mark_role_id: RoleId,
    jail_main_role_id: RoleId,

    variables: DashMap<String, EvalResult>,
    ai: ai::AI,
    chat_log: DashMap<ChannelId, Mutex<VecDeque<Message>>>,
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
        if let Err(why) = self.channel_ids[4].say(&ctx.http, "おはようっ！").await {
            error!("Error sending message: {:?}", why);
        };

        // roles のいずれかが付いているユーザーを恩赦
        let guild = self.guild_id;
        let roles = vec![self.jail_mark_role_id, self.jail_main_role_id];
        let members = guild.members(&ctx.http, None, None).await.unwrap();
        for member in members {
            if member.roles.iter().any(|role| roles.contains(role)) {
                unjail(&self.channel_ids[4], &ctx, &member.user.id, &guild, &roles).await;
            }
        }
    }
}

//direct message
async fn direct_message(bot: &Bot, ctx: &Context, msg: &Message) {
    // if message is from bot, ignore
    if msg.author.bot {
        return;
    }
    // ユーザーがこっちにきてはいけないに存在しない場合は無視
    if bot
        .guild_id
        .member(&ctx.http, &msg.author.id)
        .await
        .is_err()
    {
        return;
    }

    // get user data
    let mut user = bot.userdata.entry(msg.author.id).or_insert(UserData {
        room_pointer: bot.channel_ids[0],
        is_erogaki: false,
    });
    // update user data
    let user = update_user(bot, ctx, &mut user, &msg.author.id)
        .await
        .unwrap();

    // if message is not command, forward to the room
    if !msg.content.starts_with('!') {
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
        "help" | "たすけて" | "助けて" => help(dm, ctx, &msg.author.id, &bot.guild_id).await,
        "ping" => ping(dm, ctx).await,
        "calc" => calc(dm, ctx, command_args.join(" "), bot).await,
        "var" => var(dm, ctx, command_args.join(" "), bot).await,
        "varbulk" => varbulk(dm, ctx, command_args.join(" "), bot).await,

        // Unknown command
        _ => {
            dm.say(&ctx.http, "しらないコマンドだよ").await.unwrap();
        }
    }
}

async fn guild_message(bot: &Bot, ctx: &Context, msg: &Message) {
    let channel_id = msg.channel_id;

    if bot.chat_log.get(&channel_id).is_none() {
        bot.chat_log.insert(channel_id, Mutex::new(VecDeque::new()));
    }

    if let Ok(mut chat_log) = bot.chat_log.get(&channel_id).unwrap().lock() {
        if chat_log.len() > 100 {
            chat_log.pop_front();
        }
        chat_log.push_back(msg.clone());
    }

    // if message is from bot, ignore
    if msg.author.bot {
        return;
    }

    // if message does not contains any command, ignore
    let command_pattern =
        Regex::new(r"(?ms)((?:まなみ(?:ちゃん)?(?:\s|、|は|って|の)?)|!)(.*)").unwrap();
    let (_prefix, input_string): (String, String) = match command_pattern.captures(&msg.content) {
        Some(caps) => (
            caps.get(1).unwrap().as_str().to_owned(),
            caps.get(2).unwrap().as_str().to_owned(),
        ),
        None => return,
    };

    // get user data
    let mut user = bot.userdata.entry(msg.author.id).or_insert(UserData {
        room_pointer: bot.channel_ids[0],
        is_erogaki: false,
    });
    // update user data
    update_user(bot, ctx, &mut user, &msg.author.id)
        .await
        .unwrap();

    // dice command
    match parser::parse_dice(&input_string).finish() {
        Ok((_, parsed)) => {
            dice(&msg.channel_id, ctx, parsed).await;
            return;
        }
        Err(Error { code, .. }) => {
            if code == ErrorKind::MapRes {
                msg.channel_id
                    .say(&ctx.http, "数字がおかしいよ")
                    .await
                    .unwrap();
                return;
            }
        }
    };

    // handle other command
    let split_message = input_string.split_whitespace().collect::<Vec<&str>>();
    let command_name = split_message[0].trim();
    let command_args = &split_message[1..];
    let reply_channel = &msg.channel_id;

    match command_name {
        "help" | "たすけて" | "助けて" => {
            help(reply_channel, ctx, &msg.author.id, &bot.guild_id).await;
        }
        "isprime" => isprime(reply_channel, ctx, command_args).await,
        "calc" => calc(reply_channel, ctx, command_args.join(" "), bot).await,
        "var" => var(reply_channel, ctx, command_args.join(" "), bot).await,
        "varbulk" => varbulk(reply_channel, ctx, command_args.join(" "), bot).await,
        "cclemon" => cclemon(reply_channel, ctx, msg.author.id, command_args).await,
        "jail" => jail_main(reply_channel, ctx, command_args, bot).await,
        "unjail" => unjail_main(reply_channel, ctx, command_args, bot).await,
        "clear" | "全部忘れて" => forget_channel_log(reply_channel, ctx, bot).await,
        // Unknown command
        _ => {
            if msg.content.starts_with('!') {
                reply_channel
                    .say(&ctx.http, "しらないコマンドだよ")
                    .await
                    .unwrap();
            } else {
                // まなみが自由に応答するコーナー
                let query = bot
                    .chat_log
                    .get(&channel_id)
                    .unwrap()
                    .lock()
                    .unwrap_or(Mutex::new(VecDeque::new()).lock().unwrap())
                    .iter()
                    .map(|m| {
                        ai::Query::from_message(
                            m.author.global_name.as_ref().unwrap_or(&m.author.name),
                            &m.content,
                        )
                    })
                    .collect();
                let response = bot.ai.generate(query).await;
                let content = match response {
                    Ok(response) => response,
                    Err(e) => e,
                };

                reply_channel.say(&ctx.http, content).await.unwrap();
            }
        }
    }
}

async fn update_user<'a>(
    bot: &Bot,
    ctx: &Context,
    user: &'a mut UserData,
    author: &UserId,
) -> Result<&'a mut UserData, anyhow::Error> {
    // erogaki role check
    user.is_erogaki = author
        .to_user(&ctx.http)
        .await?
        .has_role(&ctx.http, bot.guild_id, bot.erogaki_role_id)
        .await?;
    Ok(user)
}

// commands

// help command
async fn help(reply: &ChannelId, ctx: &Context, user_id: &UserId, guild: &GuildId) {
    // 文章
    let about_me = "# まなみの自己紹介だよ！\n";
    let about_ghostwrite = "## 代筆機能があるよ！\nまなみは代筆ができるよ！　DMに送ってもらったメッセージを`!channel`で指定されたチャンネルに転送するよ！\n";

    let about_dm = "## まなみはDMでコマンドを受け付けるよ！
```
!channel        代筆先のチャンネルについてだよ
!erocheck       あなたがエロガキかどうかを判定するよ
!help           このヘルプを表示するよ
!ping           pong!
```
";

    let about_guild = "## まなみはグループチャットでコマンドを受け付けるよ！
```
![n]d<m>             m面ダイスをn回振るよ
!help                このヘルプを表示するよ
!isprime <n>         nが素数かどうかを判定するよ
!calc <expr>         数式を計算するよ
!var <name>=<expr>   calcで使える変数を定義するよ
!varbulk <codeblock> ;区切りで複数の変数を一度に定義するよ
!jail <user> [sec]   不届き者を収監して 見せます・袋とじ・管理 以外のカテゴリで喋れなくするよ
!unjail <user>       収監を解除するよ
!cclemon <opponent>  CCレモンをするよ
!clear               コマンドを実行したチャンネルのログを忘れるよ
```
";

    let mut content = MessageBuilder::new();
    content.push(about_me).push(about_ghostwrite);

    // ユーザーがこっちにきてはいけないに存在する場合はDMの話もする
    if GuildId::from(guild)
        .member(&ctx.http, user_id)
        .await
        .is_ok()
    {
        content.push(about_dm);
    }

    content.push(about_guild);
    let content = content.build();

    reply.say(&ctx.http, content).await.unwrap();
}

// change channel
async fn channel(
    reply: &ChannelId,
    ctx: &Context,
    args: &[&str],
    channels: &[ChannelId],
    user: &mut UserData,
) {
    // 引数なしの場合はチャンネル一覧を表示
    if args.is_empty() {
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
    let Ok(selector) = args[0].parse::<usize>() else {
        reply.say(&ctx.http, "IDは数字で指定してね").await.unwrap();
        return;
    };
    if selector >= channels.len() {
        reply
            .say(&ctx.http, "しらないチャンネルだよ")
            .await
            .unwrap();
        return;
    }

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
async fn dice(reply: &ChannelId, ctx: &Context, parsed: parser::Dice) {
    // パース
    let parser::Dice { num, dice, cmp } = parsed;

    // 入力のチェック
    if num > 1000000 {
        reply
            .say(&ctx.http, "そんないっぱい振れないよ")
            .await
            .unwrap();
        return;
    } else if num == 0 {
        reply.say(&ctx.http, "じゃあ振らないよ").await.unwrap();
        return;
    }

    // ダイスロール
    let mut sum = 0;
    let mut vec = vec![];
    for _ in 0..num {
        let r = rand::random::<u64>() % dice + 1;
        vec.push(r.to_string());
        sum += u128::from(r);
    }
    // 結果
    let roll_result = format!("{}D{} -> {}", num, dice, sum);
    // 内訳
    let roll_items = format!(" ({})", vec.join(", "));

    // 比較オプション
    let operation_result = cmp.map(|(operator, operand)| {
        let is_ok = parser::cmp_with_operator(&operator, sum, operand);
        let is_ok = if is_ok { "OK" } else { "NG" };
        format!(" {} {} -> {}", Into::<&str>::into(operator), operand, is_ok)
    });

    // メッセージの生成と送信
    let mut res = MessageBuilder::new();
    res.push(roll_result);
    if 1 < dice && roll_items.len() <= 100 {
        res.push(roll_items);
    }
    if let Some(operation_result) = operation_result {
        res.push(operation_result);
    }
    reply.say(&ctx.http, &res.build()).await.unwrap();
}

// erogaki status check
async fn erocheck(reply: &ChannelId, ctx: &Context, is_erogaki: bool) {
    let content = if is_erogaki {
        "エロガキ！！！！"
    } else {
        "エロガキじゃないよ"
    };
    reply.say(&ctx.http, content).await.unwrap();
}

async fn isprime(reply: &ChannelId, ctx: &Context, command_args: &[&str]) {
    if command_args.len() != 1 {
        reply
            .say(&ctx.http, "使い方: `!isprime <number>`")
            .await
            .unwrap();
        return;
    }

    let Ok(num) = command_args[0].parse::<u64>() else {
        reply.say(&ctx.http, "わかんないよ").await.unwrap();
        return;
    };

    let (is_prime, factor) = match num {
        0 | 1 => (false, vec![]),
        2 => (true, vec![2]),
        _ => {
            let mut num = num;
            let mut factor = vec![];

            while num % 2 == 0 {
                num /= 2;
                factor.push(2);
            }

            let mut i = 3;

            while i * i <= num {
                if num % i == 0 {
                    num /= i;
                    factor.push(i);
                } else {
                    i += 2;
                }
            }

            if num != 1 {
                factor.push(num);
            }

            if factor.len() == 1 {
                (true, factor)
            } else {
                (false, factor)
            }
        }
    };

    let is_prime = format!(
        "{}は{}",
        num,
        if is_prime {
            "素数だよ".to_owned()
        } else if factor.is_empty() {
            "素数じゃないよ。あたりまえでしょ？".to_owned()
        } else {
            format!("素数じゃないよ。素因数は{:?}だよ", factor)
        }
    );
    reply.say(&ctx.http, is_prime).await.unwrap();
}

const VAR_DEFAULT: &str = "_";

async fn calc(reply: &ChannelId, ctx: &Context, expression: String, bot: &Bot) {
    var_main(reply, ctx, VAR_DEFAULT.to_owned(), expression, bot).await;
}

async fn var(reply: &ChannelId, ctx: &Context, input: String, bot: &Bot) {
    let split: Vec<&str> = input.split('=').collect();
    let (var, expression) = if split.len() < 2 {
        (VAR_DEFAULT.to_owned(), input)
    } else {
        (split[0].trim().to_owned(), split[1..].join("="))
    };
    var_main(reply, ctx, var, expression, bot).await;
}

async fn varbulk(reply: &ChannelId, ctx: &Context, input: String, bot: &Bot) {
    let code_pattern = Regex::new(r"```[a-zA-Z0-9]*(.*)```").unwrap();

    //get input in code block
    let input = match code_pattern.captures(&input) {
        Some(caps) => caps.get(1).unwrap().as_str().to_owned(),
        None => return,
    };
    println!("{}", input);
    let split: Vec<&str> = input.split(';').collect();
    for s in split {
        if s.trim().is_empty() {
            continue;
        }
        var(reply, ctx, s.to_owned(), bot).await;
    }
}

async fn var_main(reply: &ChannelId, ctx: &Context, var: String, expression: String, bot: &Bot) {
    let result = calculator::eval_from_str(&expression, &bot.variables);
    match result {
        Ok(result) => {
            bot.variables.insert(var.to_string(), result.clone());
            reply.say(&ctx.http, result.to_string()).await.unwrap();
        }
        Err(e) => {
            reply
                .say(&ctx.http, format!("{} ……だってさ。", e))
                .await
                .unwrap();
        }
    }
}

async fn cclemon(reply: &ChannelId, ctx: &Context, author_id: UserId, command_args: &[&str]) {
    if command_args.len() != 1 {
        reply
            .say(&ctx.http, "使い方: `!cclemon <相手>`")
            .await
            .unwrap();
        return;
    }
    let Some(opponent_id) = parse_user_mention(command_args[0]) else {
        reply
            .say(&ctx.http, "相手をメンションで指定してね")
            .await
            .unwrap();
        return;
    };
    cclemon::cclemon(reply, ctx, (author_id, opponent_id)).await;
}

const JAIL_TERM_MAX: Duration = Duration::from_secs(3600);
const JAIL_TERM_DEFAULT: Duration = Duration::from_secs(15);

fn get_userid_from_mention(mention: &str) -> Option<UserId> {
    // "<@120123018923098123>" という形式の文字列を UserId に変換
    let id_pattern = Regex::new(r"<@(\d+)>").unwrap();
    let id_str = match id_pattern.captures(mention) {
        Some(caps) => caps.get(1).unwrap().as_str(),
        None => return None,
    };
    UserId::from_str(id_str).ok()
}

async fn jail_main(reply: &ChannelId, ctx: &Context, args: &[&str], bot: &Bot) {
    let (user, jailterm) = match args {
        [user] => {
            let user = match get_userid_from_mention(user) {
                Some(user) => user,
                None => {
                    reply.say(&ctx.http, "誰？").await.unwrap();
                    return;
                }
            };
            (user, JAIL_TERM_DEFAULT)
        }
        [user, args @ ..] => {
            let user = match get_userid_from_mention(user) {
                Some(user) => user,
                None => {
                    reply.say(&ctx.http, "誰？").await.unwrap();
                    return;
                }
            };

            let expression = args.join(" ");

            let jailtermsec = {
                match calculator::eval_from_str(&expression, &bot.variables) {
                    Ok(result) => match calculator::val_as_int(&result) {
                        Some(val) => val as u64,
                        None => {
                            reply.say(&ctx.http, "刑期がおかしいよ").await.unwrap();
                            return;
                        }
                    },
                    _ => {
                        reply.say(&ctx.http, "刑期がおかしいよ").await.unwrap();
                        return;
                    }
                }
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
    jail(
        reply,
        ctx,
        &user,
        &bot.guild_id,
        &[bot.jail_mark_role_id, bot.jail_main_role_id],
        jailterm,
    )
    .await;
}

async fn unjail_main(reply: &ChannelId, ctx: &Context, args: &[&str], bot: &Bot) {
    let user = match args {
        [user] => match get_userid_from_mention(user) {
            Some(user) => user,
            None => {
                reply.say(&ctx.http, "誰？").await.unwrap();
                return;
            }
        },
        _ => {
            reply
                .say(&ctx.http, "使い方: `!unjail <user>`")
                .await
                .unwrap();
            return;
        }
    };

    unjail(
        reply,
        ctx,
        &user,
        &bot.guild_id,
        &[bot.jail_mark_role_id, bot.jail_main_role_id],
    )
    .await;
}

async fn jail(
    reply: &ChannelId,
    ctx: &Context,
    user: &UserId,
    guild: &GuildId,
    roles: &[RoleId],
    jailterm: Duration,
) {
    let member = guild.member(&ctx.http, user).await.unwrap();
    match member.add_roles(&ctx.http, roles).await {
        Ok(_) => {}
        Err(_) => {
            reply.say(&ctx.http, "収監に失敗したよ").await.unwrap();
            return;
        }
    }

    let content = format!(
        "{}を収監したよ（刑期：{}秒）",
        user.mention(),
        jailterm.as_secs()
    );
    reply.say(&ctx.http, content).await.unwrap();

    let reply = *reply;
    let ctx = ctx.clone();
    let user = *user;
    let guild = *guild;
    let roles = roles.to_vec();

    let _ = spawn(async move {
        sleep(min(jailterm, JAIL_TERM_MAX)).await;
        unjail(&reply, &ctx, &user, &guild, &roles).await;
    })
    .await;
}

async fn unjail(
    reply: &ChannelId,
    ctx: &Context,
    user: &UserId,
    guild: &GuildId,
    roles: &[RoleId],
) {
    let member = guild.member(&ctx.http, user).await.unwrap();

    if !member.roles.iter().any(|role| roles.contains(role)) {
        return;
    }

    match member.remove_roles(&ctx.http, roles).await {
        Ok(_) => {}
        Err(_) => {
            reply.say(&ctx.http, "釈放に失敗したよ").await.unwrap();
            return;
        }
    }

    let content = format!("{}を釈放したよ", user.mention());
    reply.say(&ctx.http, content).await.unwrap();
}

async fn forget_channel_log(reply: &ChannelId, ctx: &Context, bot: &Bot) {
    reply.say(&ctx.http, "1……2の……ポカン！").await.unwrap();

    if let Some(reflog) = bot.chat_log.get(reply) {
        if let Ok(mut chat_log) = reflog.lock() {
            chat_log.clear();
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

    // 取得できなければ KOCHIKITE_GUILD_ID を使う
    let guild_id = secrets.get("GUILD_ID").map_or_else(
        || GuildId::from(KOCHIKITE_GUILD_ID),
        |id| GuildId::from_str(&id).unwrap(),
    );

    let erogaki_role_id = secrets.get("EROGAKI_ROLE_ID").map_or_else(
        || RoleId::from(EROGAKI_ROLE_ID),
        |id| RoleId::from_str(&id).unwrap(),
    );

    let jail_mark_role_id = match secrets.get("JAIL_MARK_ROLE_ID") {
        Some(id) => RoleId::from_str(&id).unwrap(),
        _ => RoleId::from(JAIL_MARK_ROLE_ID),
    };

    let jail_main_role_id = match secrets.get("JAIL_MAIN_ROLE_ID") {
        Some(id) => RoleId::from_str(&id).unwrap(),
        _ => RoleId::from(JAIL_MAIN_ROLE_ID),
    };

    let variables = DashMap::new();

    let ai = ai::AI::new(&secrets.get("AI_API_KEY").unwrap());
    let chat_log = DashMap::new();

    let client = Client::builder(&token, intents)
        .event_handler(Bot {
            userdata,
            channel_ids,
            guild_id,
            erogaki_role_id,
            jail_mark_role_id,
            jail_main_role_id,
            variables,
            ai,
            chat_log,
        })
        .await
        .expect("Err creating client");

    Ok(client.into())
}
