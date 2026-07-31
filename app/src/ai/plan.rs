//! 第1段(プランナー)が出す応答方針と、それを第2段(演者)向けのブリーフへ整形する処理。
//!
//! 方針は「返すかどうか」と「何をどんな調子で言うか」だけを持ち、まなみの言い回しは持たない。
//! 事実を持ち込めるのは第1段だけで、第2段は言い換えと感情表現しか足さない。
//! この分担が崩れると、演者が知識で穴を埋めて知ったかぶりを始める。

use schemars::JsonSchema;
use serde::Deserialize;

/// 第1段の出力。
///
/// OpenAI の structured output は strict モードで全プロパティを required にするため、
/// `Option` を使わない。値が無いことは空文字列と空配列で表す。
#[derive(Debug, Clone, Deserialize, JsonSchema)]
pub struct ResponsePlan {
    /// 今回返信するか
    pub should_reply: bool,
    /// 応答相手の表示名
    pub reply_to: String,
    /// 応答対象の要旨
    pub trigger: String,
    /// 直近の窓に入らない話の流れの要約
    pub context: String,
    /// 伝える内容
    pub points: Vec<PlanPoint>,
    /// 原文ママで出す語; 固有名詞、数値、コード、URL等
    pub keep_verbatim: Vec<String>,
    /// 分かっていないこと。素直に分からないと言わせるために明示
    pub unknowns: Vec<String>,
    /// 触れないこと。相手の嫌いな話題や、蒸し返したくない話
    pub avoid: Vec<String>,
    /// 相手の呼び方。プロフィールの希望する呼び名を反映する
    pub address_as: String,
    /// 感情の調子
    pub tone: Tone,
    /// tone だけでは表せないニュアンス。「呆れつつ面白がっている」など
    pub tone_note: String,
    /// 雑談か説明か
    pub format: Format,
}

/// 応答に含める内容ひとつ。
#[derive(Debug, Clone, Deserialize, JsonSchema)]
pub struct PlanPoint {
    /// 伝える内容。まなみの言い回しではなく、事実と意図を素で書く。
    pub content: String,
    /// 根拠の出所。ツール名、`知識`、`推測` のいずれか。
    pub source: String,
}

/// 応答の感情の調子。自由記述にすると揺れるので列挙で持つ。
// 派生スキーマを平坦な enum に保つため、各値に doc コメントを付けない
// (schemars は説明付きの値を oneOf + const へ展開する)。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum Tone {
    Cheerful,
    Excited,
    Sympathetic,
    Curious,
    Teasing,
    Apologetic,
    Serious,
    Explaining,
}

/// 応答の形。第2段のプロンプトで文字数の目安に翻訳する。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum Format {
    Chat,
    Explanation,
}

impl Tone {
    /// ブリーフに書く日本語表記。
    pub const fn label(self) -> &'static str {
        match self {
            Self::Cheerful => "明るく元気に",
            Self::Excited => "興奮している",
            Self::Sympathetic => "共感して寄り添う",
            Self::Curious => "興味しんしん",
            Self::Teasing => "からかい気味に",
            Self::Apologetic => "申し訳なさそうに",
            Self::Serious => "まじめに",
            Self::Explaining => "説明する調子で",
        }
    }
}

impl Format {
    /// ブリーフに書く日本語表記。長さの目安まで含める。
    pub const fn label(self) -> &'static str {
        match self {
            Self::Chat => "雑談。140文字以内の1文か2文で書く",
            Self::Explanation => "説明。内容を伝えきるのに必要なだけ書く",
        }
    }
}

impl ResponsePlan {
    /// 第2段へ渡すブリーフ。中身の無い項目は落とし、読ませる量を増やさない。
    pub fn render(&self) -> String {
        let mut out = vec!["## 今回の応答方針".to_owned()];

        out.extend(field("宛先", &self.reply_to));
        out.extend(field("呼び方", &self.address_as));
        out.push(format!("- 調子: {}", self.tone_label()));
        out.push(format!("- 形式: {}", self.format.label()));

        out.extend(section("何に応答するか", &self.trigger));
        out.extend(section("会話の流れ", &self.context));

        if !self.points.is_empty() {
            out.push(String::new());
            out.push("### 伝えること(この順に)".to_owned());
            for (i, point) in self.points.iter().enumerate() {
                out.push(format!(
                    "{}. {}(根拠: {})",
                    i + 1,
                    point.content.trim(),
                    point.source.trim()
                ));
            }
        }

        out.extend(bullets("原文のまま出す語", &self.keep_verbatim));
        out.extend(bullets("分かっていないこと", &self.unknowns));
        out.extend(bullets("触れないこと", &self.avoid));

        out.join("\n")
    }

    /// 調子の表記。ニュアンスの補足があれば括弧で添える。
    fn tone_label(&self) -> String {
        let note = self.tone_note.trim();
        if note.is_empty() {
            self.tone.label().to_owned()
        } else {
            format!("{}({note})", self.tone.label())
        }
    }
}

/// 第1段が使えなかったときのfallback plan
pub fn fallback_plan(last_speaker: &str) -> ResponsePlan {
    ResponsePlan {
        should_reply: true,
        reply_to: last_speaker.to_owned(),
        trigger: String::new(),
        context: String::new(),
        points: vec![PlanPoint {
            content: "直前の発言に軽く反応する。新しい話題や事実は持ち出さない".to_owned(),
            source: "推測".to_owned(),
        }],
        keep_verbatim: Vec::new(),
        unknowns: Vec::new(),
        avoid: Vec::new(),
        address_as: String::new(),
        tone: Tone::Cheerful,
        tone_note: String::new(),
        format: Format::Chat,
    }
}

/// `- ラベル: 値` の1行。値が空なら何も返さない。
fn field(label: &str, value: &str) -> Option<String> {
    let value = value.trim();
    (!value.is_empty()).then(|| format!("- {label}: {value}"))
}

/// 空行、見出し、本文の3行。本文が空なら何も返さない。
fn section(heading: &str, body: &str) -> Vec<String> {
    let body = body.trim();
    if body.is_empty() {
        return Vec::new();
    }
    vec![String::new(), format!("### {heading}"), body.to_owned()]
}

/// 空行、見出し、箇条書き。空の要素は捨て、全部空なら何も返さない。
fn bullets(heading: &str, items: &[String]) -> Vec<String> {
    let items: Vec<&str> = items
        .iter()
        .map(|item| item.trim())
        .filter(|item| !item.is_empty())
        .collect();
    if items.is_empty() {
        return Vec::new();
    }

    let mut out = vec![String::new(), format!("### {heading}")];
    out.extend(items.into_iter().map(|item| format!("- {item}")));
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn plan() -> ResponsePlan {
        ResponsePlan {
            should_reply: true,
            reply_to: "宇田".to_owned(),
            trigger: "Bluetoothの再接続が面倒だという話".to_owned(),
            context: "スピーカーを買い替えた話をしている".to_owned(),
            points: vec![PlanPoint {
                content: "自動再接続はプロファイル次第で不安定になる".to_owned(),
                source: "知識".to_owned(),
            }],
            keep_verbatim: vec!["Bluetooth".to_owned()],
            unknowns: Vec::new(),
            avoid: Vec::new(),
            address_as: "お兄ちゃん".to_owned(),
            tone: Tone::Sympathetic,
            tone_note: String::new(),
            format: Format::Chat,
        }
    }

    #[test]
    fn render_keeps_filled_sections() {
        let rendered = plan().render();
        assert!(rendered.contains("- 宛先: 宇田"));
        assert!(rendered.contains("- 呼び方: お兄ちゃん"));
        assert!(rendered.contains("- 調子: 共感して寄り添う"));
        assert!(rendered.contains("140文字以内"));
        assert!(rendered.contains("### 何に応答するか"));
        assert!(rendered.contains("1. 自動再接続はプロファイル次第で不安定になる(根拠: 知識)"));
        assert!(rendered.contains("### 原文のまま出す語"));
    }

    #[test]
    fn render_drops_empty_sections() {
        // 空の項目が見出しだけ残ると、演者に「該当なし」を読ませることになる。
        let rendered = plan().render();
        assert!(!rendered.contains("分かっていないこと"));
        assert!(!rendered.contains("触れないこと"));

        // 空白だけの要素も落とす。
        let mut p = plan();
        p.unknowns = vec!["   ".to_owned()];
        assert!(!p.render().contains("分かっていないこと"));
    }

    #[test]
    fn render_appends_tone_note_in_parens() {
        let mut p = plan();
        p.tone_note = "呆れつつ面白がっている".to_owned();
        assert!(p
            .render()
            .contains("- 調子: 共感して寄り添う(呆れつつ面白がっている)"));
    }

    #[test]
    fn fallback_replies_with_a_point_and_no_facts() {
        let p = fallback_plan("宇田");
        assert!(p.should_reply);
        assert_eq!(p.reply_to, "宇田");
        // 空の方針を演者へ渡すと、何を書けばいいのか分からなくなる。
        assert!(!p.points.is_empty());
        // 退避が事実を持ち込まないことを、根拠の出所で縛る。
        assert!(p.points.iter().all(|point| point.source == "推測"));
        assert_eq!(p.format, Format::Chat);
    }

    #[test]
    fn deserializes_a_full_plan_and_ignores_unknown_keys() {
        let json = serde_json::json!({
            "should_reply": true,
            "reply_to": "宇田",
            "trigger": "t",
            "context": "c",
            "points": [{ "content": "p", "source": "recall" }],
            "keep_verbatim": [],
            "unknowns": ["いつ買ったのか"],
            "avoid": [],
            "address_as": "お兄ちゃん",
            "tone": "teasing",
            "tone_note": "",
            "format": "explanation",
            "未知のキー": 1
        });
        let plan: ResponsePlan = serde_json::from_value(json).unwrap();
        assert_eq!(plan.tone, Tone::Teasing);
        assert_eq!(plan.format, Format::Explanation);
        assert_eq!(plan.unknowns, vec!["いつ買ったのか".to_owned()]);
    }

    /// strict モードは「全プロパティが required」を要求するので、`Option` 由来の
    /// nullable が混じると通らない。型を足したときにここで気付けるようにしておく。
    #[test]
    fn schema_has_no_nullable_field() {
        let schema = serde_json::to_string(&schemars::schema_for!(ResponsePlan)).unwrap();
        assert!(
            !schema.contains("null"),
            "nullable field in schema: {schema}"
        );
    }
}
