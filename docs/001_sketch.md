## 既存の機能

- 代筆
- LLM API呼び出し
- D1へDBを移行
- terraformの導入
- 記憶の永続化とその呼び出し（`remember` / `recall` ツール、D1 + Vectorize）
- 会話セッションの自動要約（`app/src/summarizer.rs`）

## これからの予定

- コマンド実行環境

## リポジトリ構造

```
/
├── Cargo.toml
├── Cargo.lock
├── README.md
├── CLAUDE.md
├── app/                # Discord Bot本体
├── shared/             # 共有 DTO（bot ⇄ worker の型定義）
├── workers/            # Cloudflare Workers + D1にデプロイするデータベースサービス
├── docs/
└── .github/workflows/  # デプロイ用Actions
```
