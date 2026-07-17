/// まなみのメインモジュール
/// メッセージ・コマンドのハンドリングを担当
use std::{
    sync::{Arc, Mutex},
    time::Instant,
};

use rand::rng;
use rand::{prelude::IndexedRandom, Rng};

use dashmap::DashMap;

use regex::Regex;
use serenity::all::{MessageId, MessageUpdateEvent};
use serenity::{
    all::{ActivityData, Command},
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

use calculator::EvalContext;
use commands::*;
use db::BotDatabase;

pub mod commands;

/// まなみが自発的に応答する確率の既定値 (%).
const DEFAULT_REPLY_RATE: u32 = 30;

/// チャンネル設定から自発反応の (許可, 割合%) を解決する。
/// Noneの場合、debug チャンネルでは許可、他は不許可
pub(crate) fn resolve_reply_setting(
    setting: Option<&udamanami_shared::ChannelReplySetting>,
    is_debug_channel: bool,
) -> (bool, u32) {
    let enabled = setting
        .and_then(|s| s.reply_enabled)
        .unwrap_or(is_debug_channel);
    let rate = setting
        .and_then(|s| s.reply_rate)
        .unwrap_or(DEFAULT_REPLY_RATE);
    (enabled, rate)
}

pub mod ai;
pub mod calculator;
pub mod cclemon;
pub mod db;
pub mod parser;
pub mod summarizer;

pub struct Bot {
    // Discordサーバーの情報
    // サーバーID
    pub guild_id: GuildId,

    // 代筆先が未設定のユーザーの既定の送信先
    pub default_channel_id: ChannelId,
    pub debug_channel_id: ChannelId,

    // まなみの情報
    // まなみのバージョン情報
    pub commit_hash: Option<String>,
    pub commit_date: Option<String>,

    // まなみの雑談用のAI
    pub ai: ai::ManamiAi,

    // コマンド用のデータ
    // ログなどを保存するDB
    pub database: BotDatabase,

    // 全レスモードのデータ
    pub reply_to_all_mode: Arc<Mutex<ReplyToAllModeData>>,

    // 有効なコマンドのデータ
    pub slash_commands: Vec<ManamiSlashCommand>,
    pub prefix_commands: Vec<ManamiPrefixCommand>,

    // jailコマンドのデータ
    pub jail_process: Arc<DashMap<UserId, (usize, Instant)>>,
    pub jail_id: Arc<Mutex<usize>>,
    // Jail用のRoleのID
    pub jail_mark_role_id: RoleId,
    pub jail_main_role_id: RoleId,

    // var, calcコマンドのデータ
    pub variables: EvalContext,
}

impl Bot {
    #[allow(clippy::too_many_arguments)]
    pub async fn new(
        default_channel_id: ChannelId,
        debug_channel_id: ChannelId,

        guild_id: GuildId,
        jail_mark_role_id: RoleId,
        jail_main_role_id: RoleId,

        ai: ai::ManamiAi,

        commit_hash: Option<String>,
        commit_date: Option<String>,

        disabled_commands: &[&str],

        database: BotDatabase,
    ) -> Self {
        let variables = database.retrieve_eval_context().await;
        let jail_process = Arc::new(DashMap::new());
        let jail_id = Arc::new(Mutex::new(0));
        let reply_to_all_mode: Arc<Mutex<ReplyToAllModeData>> =
            Arc::new(Mutex::new(ReplyToAllModeData::blank()));
        let prefix_commands = prefix_commands(disabled_commands);
        let slash_commands = slash_commands(disabled_commands);

        Self {
            jail_process,
            jail_id,
            default_channel_id,
            debug_channel_id,
            guild_id,
            jail_mark_role_id,
            jail_main_role_id,
            commit_hash,
            commit_date,
            variables,
            reply_to_all_mode,
            ai,
            prefix_commands,
            slash_commands,
            database,
        }
    }

    pub async fn get_user_room_pointer(&self, user_id: &UserId) -> ChannelId {
        self.database
            .fetch_user_room_pointer(user_id, self.default_channel_id)
            .await
            .unwrap_or(self.default_channel_id)
    }

    pub async fn change_room_pointer(
        &self,
        userid: &UserId,
        username: &str,
        room_pointer: ChannelId,
        channel_name: &str,
    ) -> Result<(), anyhow::Error> {
        // room_pointer は channel テーブルへの外部キーなので、
        // まだメッセージが保存されていないチャンネルでも失敗しないよう先に upsert する。
        self.database
            .upsert_channel(&room_pointer, channel_name, false)
            .await?;
        self.database
            .set_user_room_pointer(userid, username, Some(room_pointer))
            .await
    }
}

#[async_trait]
impl EventHandler for Bot {
    async fn ready(&self, ctx: Context, ready: Ready) {
        info!("{} is connected!", ready.user.name);

        if let (Some(commit_hash), Some(commit_date)) =
            (self.commit_hash.clone(), self.commit_date.clone())
        {
            ctx.set_activity(Some(ActivityData::custom(format!(
                "commit: {commit_hash} ({commit_date})"
            ))));
        };

        let message_hello_list = [
            "おはようっ！",
            "おはよーっ！！",
            "おーはよっ！",
            "おはよーっ、みんな！",
            "やっほー！",
            "やっほーっ！",
            "ふわぁ、今日もがんばるよー！",
            "ふわ〜あ、今日もがんばろー！",
            "おはよっ、今日もがんばろうねー！",
            "あれっ？　寝ちゃってた？",
            "むにゃ、寝ちゃってたぁ……",
            "おすし買ってきたよー！",
            "ピザ買ってきたよー！",
            "たーだいまーっ！",
            "まなみちゃん、参上！",
            "おはよう、みんな！",
            "だれかいるー？",
            "まなみは元気ですっ！　あれ……？",
        ];

        let message_hello = *message_hello_list.choose(&mut rng()).unwrap();

        if let Err(why) = self.debug_channel_id.say(&ctx.http, message_hello).await {
            error!("Error sending message: {:?}", why);
        };

        // ローカルコマンドの登録
        let _ = self
            .guild_id
            .set_commands(
                &ctx.http,
                self.slash_commands
                    .iter()
                    .filter(|cmd| cmd.is_local_command)
                    .map(|cmd| (cmd.register)())
                    .collect::<Vec<_>>(),
            )
            .await;

        // グローバルコマンドの登録
        for command in self
            .slash_commands
            .iter()
            .filter(|cmd| !cmd.is_local_command)
        {
            let _ = Command::create_global_command(&ctx.http, (command.register)()).await;
        }

        // roles のいずれかが付いているユーザーを恩赦
        let guild = self.guild_id;
        let roles = [self.jail_mark_role_id, self.jail_main_role_id];
        let members = guild.members(&ctx.http, None, None).await.unwrap();

        for member in members {
            if member.roles.iter().any(|role| roles.contains(role)) {
                unjail::unjail_and_notify(
                    self.debug_channel_id,
                    &ctx,
                    member.user.id,
                    &guild,
                    &roles,
                    None,
                    &self.jail_process,
                )
                .await;
            }
        }
    }

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

    async fn message_update(
        &self,
        _: Context,
        _: Option<Message>,
        new: Option<Message>,
        _: MessageUpdateEvent,
    ) {
        if let Some(new) = new {
            if let Err(e) = self.database.update_guild_message(&new).await {
                error!("Error updating message: {e:?}");
            }
        }
    }

    async fn message_delete(
        &self,
        _: Context,
        _: ChannelId,
        deleted_message_id: MessageId,
        _: Option<GuildId>,
    ) {
        if let Err(e) = self
            .database
            .delete_guild_message(&deleted_message_id)
            .await
        {
            error!("Error deleting message: {e:?}");
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
            // LLM を使うコマンドは 3 秒の応答期限に間に合わないことがあるので、
            // 先に defer してから後で本文を edit する。
            if let Err(why) = command
                .create_response(
                    &ctx.http,
                    CreateInteractionResponse::Defer(CreateInteractionResponseMessage::new()),
                )
                .await
            {
                error!("Error deferring interaction: {:?}", why);
                return;
            }

            let content = match self
                .slash_commands
                .iter()
                .find(|cmd| cmd.name == command.data.name)
            {
                Some(cmd) => (cmd.run)(command.data.options(), command_context).await,
                None => "知らないコマンドだよ！".to_owned(),
            };

            // Discord は空（または空白のみ）の content を拒否するので、そのときは代替文言を送る。
            let content = if content.trim().is_empty() {
                "（返す言葉が見つからなかったよ）".to_owned()
            } else {
                content
            };

            let builder = serenity::builder::EditInteractionResponse::new().content(content);
            if let Err(why) = command.edit_response(&ctx.http, builder).await {
                error!("Error sending message: {:?}", why);
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
        // 代筆先のチャンネルにメッセージを転送する
        let room_pointer = bot.get_user_room_pointer(&msg.author.id).await;
        if let Err(why) = room_pointer.say(&ctx.http, &msg.content).await {
            error!("Error sending message: {:?}", why);
        };
        return;
    }

    let command_context = CommandContext::new(bot, ctx, msg, msg.content[1..].to_string());

    // if message is command, handle command
    let command_name = &command_context.command_name()[..];

    match bot
        .prefix_commands
        .iter()
        .filter(|cmd| cmd.is_dm_command)
        .find(|cmd| cmd.name == command_name || cmd.alias.contains(&command_name))
    {
        Some(cmd) => (cmd.run)(command_context).await,
        None => {
            let _ = &msg
                .channel_id
                .say(&ctx.http, "しらないコマンドだよ")
                .await
                .unwrap();
        }
    }
}

async fn save_guild_message(bot: &Bot, ctx: &Context, msg: &Message) {
    let user_name = msg.author_nick(&ctx.http).await;
    let user_name = user_name
        .as_deref()
        .unwrap_or_else(|| msg.author.display_name());
    let channel_name = msg.channel_id.name(&ctx.http).await;
    let channel_name = channel_name.as_deref().unwrap_or("");

    if let Err(e) = bot
        .database
        .insert_guild_message(msg, user_name, channel_name)
        .await
    {
        error!("Error adding message: {e:?}");
    }

    // AIのためにメッセージを保存する。会話バッファはチャンネルごとに分かれるので全チャンネルで積む。
    // ボット（まなみ自身を含む）の発言は積まない。まなみの発言は add_model_log 側で積まれる。
    if !msg.author.bot {
        bot.ai.add_user_log(
            msg.channel_id.get(),
            user_name,
            &msg.content,
            msg.timestamp.to_utc(),
        );
    }
}

/// AI の生成結果を整形してチャンネルに送信する。
async fn say_ai_reply(ctx: &Context, msg: &Message, content: anyhow::Result<String>) {
    // 生成側（generate_with_model）で先頭の時刻・名前接頭辞は正規化済みなので、ここでは整形しない。
    let content = match content {
        Ok(content) => content,
        Err(e) => format!("Error sending message: {e:?}"),
    };
    // Discord の文字数上限を超えると say が失敗するので、上限ごとに分割して送る。
    // 送信失敗を握り潰すと「応答が無い」ように見えるため、失敗はログに残す。
    for chunk in split_for_discord(&content) {
        if let Err(why) = msg.channel_id.say(&ctx.http, chunk).await {
            error!("failed to send AI reply: {why:?}");
        }
    }
}

/// Discord の1メッセージ上限に合わせて文字列を分割する。
/// できるだけ行境界で区切り、1行だけで上限を超える場合は文字単位で強制分割する。
fn split_for_discord(content: &str) -> Vec<String> {
    const LIMIT: usize = 2000;
    let mut chunks: Vec<String> = Vec::new();
    let mut current = String::new();
    let mut current_len = 0usize; // current が保持する文字数

    for line in content.split_inclusive('\n') {
        let line_len = line.chars().count();

        // 1 行だけで上限を超えるなら、文字単位で強制的に割る。
        if line_len > LIMIT {
            if !current.is_empty() {
                chunks.push(std::mem::take(&mut current));
                current_len = 0;
            }
            for c in line.chars() {
                if current_len == LIMIT {
                    chunks.push(std::mem::take(&mut current));
                    current_len = 0;
                }
                current.push(c);
                current_len += 1;
            }
            continue;
        }

        // この行を足すと上限を超えるなら、いったん確定させてから積む。
        if current_len + line_len > LIMIT {
            chunks.push(std::mem::take(&mut current));
            current_len = 0;
        }
        current.push_str(line);
        current_len += line_len;
    }

    if !current.is_empty() {
        chunks.push(current);
    }
    chunks
}

/// 全レスモードの状態に応じて返答を生成し、チャンネルに送信する。
/// 全レスモード中なら期限を更新して全レス用モデルを、そうでなければ現在のモデルを使う。
async fn say_free_reply(
    bot: &Bot,
    ctx: &Context,
    msg: &Message,
    response_to_all: bool,
    response_to_all_model: &str,
) {
    // まなみは「いま応答している相手」= このメッセージの author に対してプロフィールを読み書きする。
    // 会話バッファはチャンネルごとに分かれ、各チャンネルが自分の文脈を持つので、
    // どのチャンネルでも応答相手（author）を対象にプロフィールを読み書きする。
    let target_user_id = msg.author.id.get().to_string();
    // 会話バッファはチャンネルごとに分かれているので、応答も必ずこのチャンネルのログだけを使う。
    let channel_id = msg.channel_id.get();
    let content = if response_to_all {
        bot.reply_to_all_mode.lock().unwrap().renew(); // 期限更新
        bot.ai
            .generate_with_model(
                response_to_all_model,
                &bot.database,
                &target_user_id,
                channel_id,
            )
            .await
    } else {
        bot.ai
            .generate(&bot.database, &target_user_id, channel_id)
            .await
    };
    say_ai_reply(ctx, msg, content).await;
}

async fn guild_message(bot: &Bot, ctx: &Context, msg: &Message) {
    save_guild_message(bot, ctx, msg).await;

    // 反応すべきメッセージかどうか確認
    if !has_privilege(bot, ctx, msg).await {
        return;
    }

    // 全レスモード中？
    let (response_to_all, response_to_all_model) = {
        let mode = bot.reply_to_all_mode.lock().unwrap();
        (mode.is_active(), mode.model.clone())
    };
    let is_debug_channel = msg.channel_id.get() == bot.debug_channel_id.get();

    // if message does not contains any command, respond with AI
    let command_pattern =
        Regex::new(r"(?ms)((?:まなみ(?:ちゃん)?(?:\s|、|は|って|の)?)|!)(.*)").unwrap();
    let input_string = match command_pattern.captures(&msg.content) {
        // コマンド部分を抽出
        Some(caps) => caps.get(2).unwrap().as_str().to_owned(),
        None => {
            // ランダム返信
            let setting = bot
                .database
                .fetch_channel_reply_setting(&msg.channel_id)
                .await
                .ok()
                .flatten();
            let (enabled, rate) = resolve_reply_setting(setting.as_ref(), is_debug_channel);
            if enabled
                && ((is_debug_channel && response_to_all)
                    || rng().random::<f32>() < rate as f32 / 100.0)
            {
                say_free_reply(bot, ctx, msg, response_to_all, &response_to_all_model).await;
            }
            return;
        }
    };

    let command_context = CommandContext::new(bot, ctx, msg, input_string);

    // handle other command
    let command_name = &command_context.command_name()[..];

    let guild_command = bot
        .prefix_commands
        .iter()
        .filter(|cmd| cmd.is_guild_command)
        .find(|cmd| cmd.name == command_name || cmd.alias.contains(&command_name));

    match guild_command {
        Some(cmd) => (cmd.run)(command_context).await,
        None => {
            // "!" 始まりで未知のコマンドなら dice にフォールバック
            if msg.content.starts_with('!') {
                if let Some(cmd) = bot
                    .prefix_commands
                    .iter()
                    .find(|cmd| cmd.is_guild_command && cmd.name == "dice")
                {
                    return (cmd.run)(command_context).await;
                }
            }

            if is_debug_channel {
                // まなみが自由に応答するコーナー
                say_free_reply(bot, ctx, msg, response_to_all, &response_to_all_model).await;
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

#[cfg(test)]
mod reply_setting_tests {
    use super::{resolve_reply_setting, DEFAULT_REPLY_RATE};
    use udamanami_shared::ChannelReplySetting;

    fn setting(reply_enabled: Option<bool>, reply_rate: Option<u32>) -> ChannelReplySetting {
        ChannelReplySetting {
            channel_id: "1".to_owned(),
            reply_enabled,
            reply_rate,
        }
    }

    #[test]
    fn unset_channel_defaults_to_debug_only() {
        // 行が無いチャンネル: debug だけが既定で許可され、他は黙る。
        assert_eq!(
            resolve_reply_setting(None, true),
            (true, DEFAULT_REPLY_RATE)
        );
        assert_eq!(
            resolve_reply_setting(None, false),
            (false, DEFAULT_REPLY_RATE)
        );
    }

    #[test]
    fn null_columns_fall_back_to_defaults() {
        // マイグレーション直後の既存行(両方 NULL)は未設定と同じ扱い。
        let s = setting(None, None);
        assert_eq!(
            resolve_reply_setting(Some(&s), false),
            (false, DEFAULT_REPLY_RATE)
        );
        assert_eq!(
            resolve_reply_setting(Some(&s), true),
            (true, DEFAULT_REPLY_RATE)
        );
    }

    #[test]
    fn explicit_setting_overrides_the_debug_default() {
        // 明示設定は debug かどうかに関わらず優先される。
        let enabled = setting(Some(true), Some(50));
        assert_eq!(resolve_reply_setting(Some(&enabled), false), (true, 50));

        let disabled = setting(Some(false), Some(50));
        assert_eq!(resolve_reply_setting(Some(&disabled), true), (false, 50));
    }

    #[test]
    fn rate_and_enabled_resolve_independently() {
        // 割合だけ設定済みなら、許可は既定(debug 判定)にフォールバックする。
        let rate_only = setting(None, Some(5));
        assert_eq!(resolve_reply_setting(Some(&rate_only), false), (false, 5));
        assert_eq!(resolve_reply_setting(Some(&rate_only), true), (true, 5));

        // 許可だけ設定済み(トグルONのみ)なら、割合は既定になる。
        let enabled_only = setting(Some(true), None);
        assert_eq!(
            resolve_reply_setting(Some(&enabled_only), false),
            (true, DEFAULT_REPLY_RATE)
        );
    }

    #[test]
    fn zero_rate_is_kept_and_never_replies() {
        // 0% は「許可されているが反応しない」。rng() < 0.0 は常に偽。
        let s = setting(Some(true), Some(0));
        assert_eq!(resolve_reply_setting(Some(&s), false), (true, 0));
    }
}

#[cfg(test)]
mod tests {
    use super::split_for_discord;

    const LIMIT: usize = 2000;

    fn chars(s: &str) -> usize {
        s.chars().count()
    }

    #[test]
    fn keeps_short_content_as_single_chunk() {
        let chunks = split_for_discord("やっほー！");
        assert_eq!(chunks, vec!["やっほー！".to_owned()]);
    }

    #[test]
    fn empty_content_yields_no_chunk() {
        assert!(split_for_discord("").is_empty());
    }

    #[test]
    fn every_chunk_is_within_the_limit_and_content_is_preserved() {
        // 改行を含み上限を大きく超える多バイト文字列。
        let line = "あ".repeat(1500) + "\n";
        let content = line.repeat(5); // 約 7505 文字
        let chunks = split_for_discord(&content);

        assert!(chunks.len() > 1);
        for chunk in &chunks {
            assert!(
                chars(chunk) <= LIMIT,
                "chunk exceeds limit: {}",
                chars(chunk)
            );
        }
        // 分割しても内容は完全に復元できる。
        assert_eq!(chunks.concat(), content);
    }

    #[test]
    fn hard_splits_a_single_line_longer_than_the_limit() {
        // 改行の無い、上限超えの1行（多バイト）。
        let content = "猫".repeat(5000);
        let chunks = split_for_discord(&content);

        assert!(chunks.len() >= 3);
        for chunk in &chunks {
            assert!(chars(chunk) <= LIMIT);
        }
        assert_eq!(chunks.concat(), content);
    }
}
