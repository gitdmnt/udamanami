//! botと workersで共有するDTO型.

use serde::{Deserialize, Serialize};

pub type MessageId = String;
pub type ChannelId = String;
pub type UserId = String;
pub type MemoryId = String;
pub type ChunkId = String;

// ---------------- メッセージ ----------------

#[derive(Debug, Deserialize, Serialize)]
pub struct Message {
    pub message_id: MessageId,
    pub channel_id: ChannelId,
    pub user_id: UserId,
    pub content: String,
    pub timestamp: String,
    /// 取得時のみ user テーブルとの JOIN で埋まる。挿入時は不要。
    #[serde(default)]
    pub username: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct UpdateMessage {
    pub message_id: MessageId,
    pub content: String,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct DeleteMessage {
    pub message_id: MessageId,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct GetMessages {
    pub channel_id: ChannelId,
    pub limit: usize,
    pub order: Option<MessageOrder>,
    pub from: Option<String>, // chrono::DateTime<chrono::Utc>
    pub to: Option<String>,
    /// Exclusive composite cursor. Old Workers ignore these query parameters.
    #[serde(default)]
    pub after: Option<String>,
    #[serde(default)]
    pub after_message_id: Option<MessageId>,
    #[serde(default)]
    pub summary_pending_only: Option<bool>,
}

#[derive(Debug, Deserialize, Serialize, PartialEq, Eq)]
pub enum MessageOrder {
    Asc,
    Desc,
}

// ---------------- チャンネル ----------------

#[derive(Debug, Deserialize, Serialize)]
pub struct Channel {
    pub channel_id: ChannelId,
    pub is_thread: bool,
    pub name: String,
}

/// チャンネルごとの自発反応設定。`None` は「未設定 → 実行時デフォルト」を表す。
/// 頻繁な [`Channel`] upsert 経路とは分離し、name/is_thread を上書きせず部分更新する。
#[derive(Debug, Deserialize, Serialize)]
pub struct ChannelReplySetting {
    pub channel_id: ChannelId,
    /// 自発反応を許可するか。
    pub reply_enabled: Option<bool>,
    /// 自発反応する割合(%)。0..=100。
    pub reply_rate: Option<u32>,
}

/// 未要約メッセージを持つチャンネルのうち、無言が続いたか、未要約が一定件数たまったものだけが返る。
#[derive(Debug, Deserialize, Serialize)]
pub struct SummarizeCandidate {
    pub channel_id: ChannelId,
    pub name: String,
    /// どこまで要約済みか。`None` は実行時デフォルトにフォールバック
    pub last_summarized_at: Option<String>,
    // Kept for old-app/new-Worker wire compatibility.
    pub first_pending_at: String,
    pub last_message_at: String,
    pub pending_count: i64,
    #[serde(default)]
    pub last_summarized_message_id: Option<MessageId>,
    #[serde(default)]
    pub first_pending_message_id: Option<MessageId>,
    #[serde(default)]
    pub last_pending_message_id: Option<MessageId>,
}

/// 自動要約の進捗。巻き戻りを防ぐため単調増加
#[derive(Debug, Deserialize, Serialize)]
pub struct SetChannelSummarized {
    pub channel_id: ChannelId,
    pub last_summarized_at: String,
    #[serde(default)]
    pub last_summarized_message_id: Option<MessageId>,
    /// IDs actually returned by GET /message. Empty means a legacy app and is
    /// intentionally a no-op in the new Worker.
    #[serde(default)]
    pub message_ids: Vec<MessageId>,
}

// ---------------- ユーザー ----------------

#[derive(Debug, Deserialize, Serialize)]
pub struct User {
    pub user_id: UserId,
    pub username: String,
    pub room_pointer: Option<ChannelId>,
}

/// まなみが応答相手について学習した人間プロフィール。
#[derive(Debug, Deserialize, Serialize)]
pub struct UserProfile {
    pub user_id: UserId,
    pub username: String,
    /// 希望する呼び名。
    pub calling_name: Option<String>,
    /// 好きな話題。
    pub liked_topics: Option<String>,
    /// 嫌いな話題。
    pub disliked_topics: Option<String>,
}

/// プロフィールの部分更新リクエスト。
#[derive(Debug, Deserialize, Serialize)]
pub struct SetUserProfile {
    pub user_id: UserId,
    pub calling_name: Option<String>,
    pub liked_topics: Option<String>,
    pub disliked_topics: Option<String>,
}

impl UserProfile {
    /// 応答相手のプロフィールをシステムプロンプト末尾へ差し込むブロックに整形する。
    /// `None` または空白のみのフィールドは行ごと省く。表示すべき項目が皆無なら空文字列を返す。
    pub fn to_prompt(&self) -> String {
        let lines: Vec<String> = [
            profile_line("名前", Some(self.username.as_str())),
            profile_line("希望する呼び名", self.calling_name.as_deref()),
            profile_line("好きな話題", self.liked_topics.as_deref()),
            profile_line("嫌いな話題", self.disliked_topics.as_deref()),
        ]
        .into_iter()
        .flatten()
        .collect();

        if lines.is_empty() {
            String::new()
        } else {
            format!(
                "## いま応答している相手のプロフィール\n{}",
                lines.join("\n")
            )
        }
    }
}

/// 値があり、trim 後に非空なら `- ラベル: 値` の 1 行を返す。それ以外は `None`。
fn profile_line(label: &str, value: Option<&str>) -> Option<String> {
    let trimmed = value?.trim();
    if trimmed.is_empty() {
        None
    } else {
        Some(format!("- {label}: {trimmed}"))
    }
}

// ---------------- calc var ----------------

#[derive(Debug, Deserialize, Serialize)]
pub struct CalcVar {
    pub var_name: String,
    pub var_value: String,
    pub user_id: UserId,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct DeleteCalcVar {
    pub var_name: String,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct CalcVarWithUsername {
    pub var_name: String,
    pub username: Option<String>,
}

// ---------------- memory ----------------

#[derive(Debug, Deserialize, Serialize)]
pub struct Memory {
    pub title: String,
    pub content: String,
    /// 記憶の出所。`None` は remember ツール由来(手動), 自動要約は `Some("auto_summary")`
    #[serde(default)]
    pub source: Option<String>,
    /// 会話が行われたチャンネル名。`None` は手動記憶。どのチャンネルの話かで内容が現実かどうかが
    /// 変わる(夢日記など)ので、本文とは別の列として持つ。
    #[serde(default)]
    pub channel_name: Option<String>,
    /// 会話が行われた日時(RFC3339)。`None` は手動記憶。作成時刻(`timestamp`)とは別で、
    /// 過去ログをまとめて要約したときは作成時刻とずれる。
    #[serde(default)]
    pub occurred_at: Option<String>,
}

/// 既存メモリ1件を新しい内容で置き換える。chunk とベクトルを作り直す。
#[derive(Debug, Deserialize, Serialize)]
pub struct UpdateMemory {
    pub memory_id: MemoryId,
    pub title: String,
    pub content: String,
}

/// 意味検索の結果1件。Vectorize の類似度 score を付与して返す。
#[derive(Debug, Deserialize, Serialize)]
pub struct MemorySearchResult {
    pub chunk_id: ChunkId,
    pub memory_id: MemoryId,
    pub chunk_index: i64,
    pub content: String,
    pub title: String,
    pub timestamp: String,
    /// 会話が行われたチャンネル名。手動記憶なら `None`。
    #[serde(default)]
    pub channel_name: Option<String>,
    /// 会話が行われた日時(RFC3339)。手動記憶なら `None`。
    #[serde(default)]
    pub occurred_at: Option<String>,
    pub score: f64,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct MemoryListItem {
    pub memory_id: MemoryId,
    pub title: String,
    pub timestamp: String,
}

/// メモリ1件の全文。chunk を連結して本文を復元したもの。
#[derive(Debug, Deserialize, Serialize)]
pub struct MemoryDetail {
    pub memory_id: MemoryId,
    pub title: String,
    pub timestamp: String,
    pub content: String,
    /// 会話が行われたチャンネル名。手動記憶なら `None`。
    #[serde(default)]
    pub channel_name: Option<String>,
    /// 会話が行われた日時(RFC3339)。手動記憶なら `None`。
    #[serde(default)]
    pub occurred_at: Option<String>,
}

// ---------------- プラットフォームの上限 ----------------

/// D1 が 1 文あたりに受け付ける bind パラメータの上限。
///
/// `d1.batch()` に入れても文ごとに個別適用されるので、batch にまとめても緩和されない。
/// 超えると prepare の時点で `too many SQL variables ... SQLITE_ERROR` になり、
/// batch はトランザクションなので同じ batch の他の文もまとめてロールバックする。
/// <https://developers.cloudflare.com/d1/platform/limits/>
pub const D1_MAX_BOUND_PARAMS: usize = 100;

/// Vectorize の `deleteByIds` 1 リクエストに載せられる id の上限。
///
/// 限界表にもAPIリファレンスにも記載が無いが、超えると API が
/// `too many ids in payload; max id count is 100 [code: 40007]` を返す(2026-09 実測)。
pub const VECTORIZE_MAX_DELETE_IDS: usize = 100;

/// 固定バインドが `fixed` 個ある文に、あと何個の可変バインドを載せられるか。
///
/// 文に述語を足してバインドが増えたときは、`fixed` を必ず一緒に増やすこと。
/// ここを更新し忘れると、上限を超えるのは実行時だけで、テストは緑のまま通る。
#[must_use]
pub const fn max_variable_bindings(fixed: usize) -> usize {
    D1_MAX_BOUND_PARAMS.saturating_sub(fixed)
}

/// 確定 UPDATE 1 文ぶんの SQL とバインド値。
#[derive(Debug, PartialEq, Eq)]
pub struct ConfirmChunk {
    pub sql: String,
    pub bindings: Vec<String>,
}

/// `PUT /channel/summary` の確定 UPDATE を、D1 のバインド上限を超えない複数の文に分割する。
///
/// 各文は channel_id で1個バインドするので、1文に載る message_id は
/// [`max_variable_bindings(1)`] 件まで。呼び出し側はこれらを cursor 文と同じ
/// `d1.batch` に入れること。batch はトランザクションなので、分割しても原子性は保たれる。
///
/// 分割しても結果は1文のときと同じになる。更新条件は行ごとに独立で、`chunks()` の
/// 返す部分集合は互いに素なので、和集合は入力の集合に一致する。
/// 行トリガ `message_summary_state_after_confirm` の発火回数も文数ではなく行数で決まる。
#[must_use]
pub fn confirm_pending_chunks(
    channel_id: &ChannelId,
    message_ids: &[MessageId],
) -> Vec<ConfirmChunk> {
    const IDS_PER_STATEMENT: usize = max_variable_bindings(1);

    message_ids
        .chunks(IDS_PER_STATEMENT)
        .map(|ids| {
            let placeholders = vec!["?"; ids.len()].join(", ");
            let mut bindings = Vec::with_capacity(ids.len() + 1);
            bindings.push(channel_id.clone());
            bindings.extend(ids.iter().cloned());
            ConfirmChunk {
                sql: format!(
                    "UPDATE message SET summary_pending = 0
                     WHERE channel_id = ? AND summary_pending = 1
                       AND message_id IN ({placeholders})"
                ),
                bindings,
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn profile(
        username: &str,
        calling_name: Option<&str>,
        liked: Option<&str>,
        disliked: Option<&str>,
    ) -> UserProfile {
        UserProfile {
            user_id: "1".to_owned(),
            username: username.to_owned(),
            calling_name: calling_name.map(str::to_owned),
            liked_topics: liked.map(str::to_owned),
            disliked_topics: disliked.map(str::to_owned),
        }
    }

    #[test]
    fn to_prompt_includes_all_present_fields() {
        let block = profile(
            "うさみむ",
            Some("おねえちゃん"),
            Some("手芸"),
            Some("ホラー"),
        )
        .to_prompt();
        assert!(block.contains("## いま応答している相手のプロフィール"));
        assert!(block.contains("- 名前: うさみむ"));
        assert!(block.contains("- 希望する呼び名: おねえちゃん"));
        assert!(block.contains("- 好きな話題: 手芸"));
        assert!(block.contains("- 嫌いな話題: ホラー"));
    }

    #[test]
    fn to_prompt_skips_missing_fields_but_keeps_name() {
        let block = profile("だれか", None, None, None).to_prompt();
        assert!(block.contains("- 名前: だれか"));
        assert!(!block.contains("希望する呼び名"));
        assert!(!block.contains("好きな話題"));
        assert!(!block.contains("嫌いな話題"));
    }

    #[test]
    fn to_prompt_treats_empty_and_whitespace_as_absent() {
        // Some("") と空白のみは学習済みとみなさず行を出さない。
        let block = profile("ゲスト", Some(""), Some("   "), None).to_prompt();
        assert!(block.contains("- 名前: ゲスト"));
        assert!(!block.contains("希望する呼び名"));
        assert!(!block.contains("好きな話題"));
    }

    #[test]
    fn to_prompt_returns_empty_when_nothing_to_show() {
        // 名前すら空なら、注入すべきブロックは無い。
        let block = profile("  ", Some(""), None, None).to_prompt();
        assert!(block.is_empty());
    }

    #[test]
    fn confirm_chunks_never_exceed_d1_bound_parameter_limit() {
        // これがこの事故で落ちるべきだったテスト。
        // 修正前は全 message_id を1文に載せていたので、n=100 で 101 バインドになり
        // D1 が prepare の時点で拒否していた。件数トリガが pending_count >= 100 なので、
        // 100..=200 は「たまたま起きうる値」ではなく「必ず通る値」である。
        for n in [0_usize, 1, 98, 99, 100, 101, 199, 200, 201, 1000] {
            let ids: Vec<MessageId> = (0..n).map(|i| format!("m{i}")).collect();
            let chunks = confirm_pending_chunks(&"c1".to_owned(), &ids);

            let mut seen: Vec<MessageId> = Vec::new();
            for chunk in &chunks {
                assert!(
                    chunk.bindings.len() <= D1_MAX_BOUND_PARAMS,
                    "n={n}: {} バインドは D1 の上限 {D1_MAX_BOUND_PARAMS} を超える",
                    chunk.bindings.len()
                );
                // プレースホルダとバインド値の個数がずれると、D1 は
                // "Wrong number of parameter bindings" で落ちる。
                assert_eq!(chunk.sql.matches('?').count(), chunk.bindings.len());
                // 先頭は channel_id、残りが message_id。
                assert_eq!(chunk.bindings[0], "c1");
                seen.extend_from_slice(&chunk.bindings[1..]);
            }
            // 取りこぼし・重複・順序の入れ替わりがあると、確定されない行が
            // 永久に pending のまま残り、同じ暴走が再発する。
            assert_eq!(seen, ids, "n={n}: 分割で ID の集合が変わった");
        }
    }

    #[test]
    fn max_variable_bindings_leaves_room_for_fixed_binds() {
        assert_eq!(max_variable_bindings(0), D1_MAX_BOUND_PARAMS);
        assert_eq!(max_variable_bindings(1), D1_MAX_BOUND_PARAMS - 1);
        // 固定バインドが上限を食い切っても 0 に飽和するだけで、panic しない。
        assert_eq!(max_variable_bindings(D1_MAX_BOUND_PARAMS + 5), 0);
    }

    #[test]
    fn confirm_chunks_are_empty_for_no_ids() {
        // 空なら文を1つも作らない。空の IN () は SQL として不正なので、
        // 呼び出し側が batch に空文を入れてしまうことを防ぐ。
        assert!(confirm_pending_chunks(&"c1".to_owned(), &[]).is_empty());
    }
}
