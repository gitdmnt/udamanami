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
