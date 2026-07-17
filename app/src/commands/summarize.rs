//! 会話セッションの自動要約を手で叩くデバッグコマンド。
//!
//! 本来の要約は summarizer の常駐タスクが「6時間の無音」か「100件の蓄積」で走らせる。
//! それを待たずに動作確認するためのもので、境界条件だけを飛ばして本番と同じ経路を通す。

use chrono::Utc;

use crate::summarizer::{self, summarize_channel, Outcome};

use super::{CommandContext, ManamiSlashCommand};

const COMMAND_NAME: &str = "summarize";
const DESCRIPTION: &str = "直近の会話を要約して記憶する";

pub const SLASH_SUMMARIZE_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: COMMAND_NAME,
    usage: "/summarize",
    description: DESCRIPTION,
    register,
    run: |_, ctx| Box::pin(async move { run_body(&ctx).await }),
    is_local_command: true,
};

pub fn register() -> serenity::builder::CreateCommand {
    serenity::builder::CreateCommand::new(COMMAND_NAME).description(DESCRIPTION)
}

async fn run_body(ctx: &CommandContext<'_>) -> String {
    let my_userid = match ctx.ctx.http.get_current_user().await {
        Ok(user) => user.id,
        Err(e) => return format!("自分が誰だか分からなくなっちゃった。Error: {e}"),
    };

    let now = Utc::now();
    let candidate = match summarizer::on_demand_candidate(ctx.bot, ctx.channel_id, now).await {
        Ok(Some(candidate)) => candidate,
        Ok(None) => return "もう全部知ってるよ！".to_owned(),
        Err(e) => return format!("チャンネル一覧の取得に失敗しちゃったよ…… Error: {e}"),
    };

    match summarize_channel(ctx.bot, my_userid, &candidate, now).await {
        Ok(Outcome::Summarized { title }) => format!("「{title}」を記憶したよ！"),
        Ok(Outcome::Skipped(reason)) => {
            format!("記憶しなかったよ（{reason}）。ここまでは要約済みにしておくね。")
        }
        Ok(Outcome::Nothing) => "もう全部知ってるよ！".to_owned(),
        Err(e) => format!("あれー、要約に失敗しちゃった…… Error: {e}"),
    }
}
