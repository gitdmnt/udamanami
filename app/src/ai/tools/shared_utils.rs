/// Wikipedia の snippet に含まれるインラインの HTML タグと実体参照を取り除き、
/// 1 行の素のテキストにする。
pub fn strip_html(html: &str) -> String {
    let mut stripped = String::with_capacity(html.len());
    let mut in_tag = false;
    for c in html.chars() {
        match c {
            '<' => in_tag = true,
            '>' => in_tag = false,
            _ if !in_tag => stripped.push(c),
            _ => {}
        }
    }
    decode_entities(&stripped)
}

/// ブロック構造を持つ HTML（Wikipedia の extract）を、見出し・箇条書き・段落の
/// 区切りを保ったプレーンテキストに変換する。
pub fn html_to_text(html: &str) -> String {
    use regex::Regex;

    // 見出しと箇条書きを Markdown 風のマーカーに、段落・改行タグを改行に置き換える。
    let text = Regex::new(r"(?i)<h[12][^>]*>")
        .unwrap()
        .replace_all(html, "\n\n## ");
    let text = Regex::new(r"(?i)<h[3-6][^>]*>")
        .unwrap()
        .replace_all(&text, "\n\n### ");
    let text = Regex::new(r"(?i)<li[^>]*>")
        .unwrap()
        .replace_all(&text, "\n- ");
    let text = Regex::new(r"(?i)</p>|</h[1-6]>|<br\s*/?>")
        .unwrap()
        .replace_all(&text, "\n");
    // 残りのタグを除去する。
    let text = Regex::new(r"<[^>]*>").unwrap().replace_all(&text, "");
    let text = decode_entities(&text);

    // 各行を trim し、空行の連続を 1 行にまとめる。
    // 箇条書きは直前の空行を詰めて、見出しや前の項目に密着させる。
    let mut lines: Vec<&str> = Vec::new();
    for line in text.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            if lines.last().is_some_and(|l| l.is_empty()) {
                continue;
            }
        } else if trimmed.starts_with("- ") && lines.last().is_some_and(|l| l.is_empty()) {
            lines.pop();
        }
        lines.push(trimmed);
    }
    lines.join("\n").trim().to_owned()
}

pub fn decode_entities(text: &str) -> String {
    text.replace("&lt;", "<")
        .replace("&gt;", ">")
        .replace("&quot;", "\"")
        .replace("&#039;", "'")
        .replace("&#39;", "'")
        .replace("&nbsp;", " ")
        .replace("&amp;", "&")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strip_html_removes_tags_and_entities() {
        // Wikipedia の snippet に含まれる searchmatch タグは取り除かれる。
        assert_eq!(
            strip_html("<span class=\"searchmatch\">POPOPO</span>株式会社"),
            "POPOPO株式会社"
        );
        // 実体参照はデコードされる。&amp; の二重デコードも起きない。
        assert_eq!(
            strip_html("a &amp;lt; b &quot;c&quot; &#39;d&#39;"),
            "a &lt; b \"c\" 'd'"
        );
    }

    #[test]
    fn html_to_text_preserves_block_structure() {
        let html = "<p><b>POPOPO株式会社</b>は日本のIT企業。\n</p>\
            <h2 data-mw-anchor=\"概要\">概要</h2>\n<p>SNSを運営する。\n</p>\
            <h3 data-mw-anchor=\"タイプアップ\">タイプアップ</h3>\
            <ul><li>すとぷり</li>\n<li>東方Project</li></ul>";
        assert_eq!(
            html_to_text(html),
            "POPOPO株式会社は日本のIT企業。\n\n## 概要\n\nSNSを運営する。\n\n### タイプアップ\n- すとぷり\n- 東方Project"
        );
    }
}
