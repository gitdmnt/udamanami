use regex::Regex;
use rig::completion::AssistantContent;

/// 応答ストリームを、種類ごとの連続ブロックへ圧縮した中間表現。
pub enum Block {
    Text(String),
    Reasoning(String),
    // ツール使用の痕跡表示用。結果全文は Discord の文字数上限を超えるので載せず、名前と引数だけ残す。
    ToolCall { name: String, args: String },
}

impl Block {
    /// ブロックを Discord 向けの文字列へ装飾する。
    pub fn decorate(self) -> String {
        match self {
            Self::Text(text) => text,
            Self::Reasoning(text) => prefix_lines(&text, "> -# "),
            Self::ToolCall { name, args } => {
                prefix_lines(&format!("ツール {name} を使ったよ (引数: {args})"), "> -# ")
            }
        }
    }
}

/// 連続する同種の content をひとつのブロックにまとめる（圧縮）。
pub fn compress(choice: impl IntoIterator<Item = AssistantContent>) -> Vec<Block> {
    choice.into_iter().fold(Vec::new(), |mut blocks, content| {
        match content {
            AssistantContent::Text(t) => match blocks.last_mut() {
                Some(Block::Text(buf)) => buf.push_str(&t.text),
                _ => blocks.push(Block::Text(t.text)),
            },
            AssistantContent::Reasoning(r) => {
                let text = r.display_text();
                match blocks.last_mut() {
                    Some(Block::Reasoning(buf)) => {
                        buf.push('\n');
                        buf.push_str(&text);
                    }
                    _ => blocks.push(Block::Reasoning(text)),
                }
            }
            AssistantContent::ToolCall(_) => {} // ツール呼び出しは AgentRunStep::CallTools で処理する
            AssistantContent::Image(_) => {}
        }
        blocks
    })
}

/// 各行の先頭に prefix を付ける。
fn prefix_lines(text: &str, prefix: &str) -> String {
    text.lines()
        .map(|line| format!("{prefix}{line}"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// まなみの返信が履歴を模倣して先頭へ付けてしまう接頭辞を剥がす。
/// (1) 先頭の時刻ブロック `[YYYY-MM-DD HH:MM]` を除去し、
/// (2) 続く `まなみ:` / `うだまなみ:`（全角・半角コロン）を、**コロン付きのときだけ**除去する。
/// コロン必須にすることで「まなみだよ！」のような正当な本文を誤って削らない。
pub fn strip_leading_prefix(text: &str) -> String {
    // 生成ごとに1回しか通らないので、既存流儀（lib.rs）に倣い都度コンパイルする。
    let stamp = Regex::new(r"^\s*\[\d{4}-\d{2}-\d{2} \d{2}:\d{2}\]\s*").unwrap();
    let name = Regex::new(r"^\s*(?:うだまなみ|まなみ)\s*[:：]\s*").unwrap();

    let without_stamp = stamp.replace(text, "");
    let without_name = name.replace(&without_stamp, "");
    without_name.into_owned()
}

#[cfg(test)]
mod tests {
    use super::strip_leading_prefix;

    #[test]
    fn strips_timestamp_and_name_prefix() {
        assert_eq!(
            strip_leading_prefix("[2026-07-17 12:34] まなみ: やっほー"),
            "やっほー"
        );
        assert_eq!(
            strip_leading_prefix("[2026-07-17 12:34] やっほー"),
            "やっほー"
        );
        assert_eq!(strip_leading_prefix("うだまなみ: やっほー"), "やっほー");
    }

    #[test]
    fn keeps_legitimate_body_starting_with_name() {
        // コロンが無ければ本文とみなして残す。
        assert_eq!(strip_leading_prefix("まなみだよ！"), "まなみだよ！");
        assert_eq!(strip_leading_prefix("やっほー"), "やっほー");
    }

    #[test]
    fn strips_double_timestamp() {
        // 二重に焼かれても先頭ブロックを1つ剥がし、名前を消せば本文が残る。
        assert_eq!(
            strip_leading_prefix("[2026-07-17 12:34] まなみ: 本文"),
            "本文"
        );
    }
}
