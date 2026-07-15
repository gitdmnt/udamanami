//! 利用可能な LLM モデル一覧の解決。

/// `LLM_MODELS` が未設定・空のときに使う既定モデル一覧。
const DEFAULT_MODELS: &[&str] = &["gpt-5.4-mini", "gpt-5.4-nano", "gpt-5.6-luna"];

/// 利用可能なモデル一覧を環境変数 `LLM_MODELS`（カンマ区切り）から得る。
/// 未設定または空のときは [`DEFAULT_MODELS`] にフォールバックする。
/// スラッシュコマンドの選択肢と既定モデルの決定に使う。
pub fn available_models() -> Vec<String> {
    let from_env: Vec<String> = std::env::var("LLM_MODELS")
        .unwrap_or_default()
        .split(',')
        .map(|m| m.trim().to_owned())
        .filter(|m| !m.is_empty())
        .collect();

    if from_env.is_empty() {
        DEFAULT_MODELS.iter().map(|s| (*s).to_owned()).collect()
    } else {
        from_env
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn available_models_never_empty() {
        // 環境変数の有無にかかわらず、少なくとも既定モデルが返る。
        assert!(!available_models().is_empty());
    }
}
