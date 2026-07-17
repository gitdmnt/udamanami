# udamanami

Discord ボット「うだまなみ」の実装です。

serenity でメッセージとスラッシュコマンドを処理し、雑談には OpenAI 互換の
LLM（rig 経由）を、ログや変数の保存には SQLite（sea-orm）を使います。
使うモデルは環境変数と `/model` コマンドで切り替えられます。
Discord への外向き接続だけで動き、受信ポートは開けません。

## ローカルでの実行

必要なツールは Rust（stable）です。
設定はすべて環境変数で渡します（`.env` の自動読み込みはしません）。
`.env.example` を `.env` にコピーして埋めるのが楽です。

```sh
export DISCORD_TOKEN=...        # 必須
export DISCORD_GUILD_ID=...     # 必須
export LLM_API_KEY=...          # 必須（OpenAI などの API キー）
export LLM_MODELS=5.4-mini,5.4-nano,5.6-luna  # 選べるモデル（カンマ区切り）
export DEFAULT_CHANNEL_ID=...   # 必須（代筆先が未設定のときの既定の送信先）
export DEBUG_ROOM_ID=...        # AI 雑談・起動あいさつを流すチャンネル
# 任意: LLM_MODEL（既定モデル）, LLM_BASE_URL（既定は OpenAI）
# 任意: JAIL_MARK_ROLE_ID, JAIL_MAIN_ROLE_ID, DISABLED_COMMANDS
# 任意: DATABASE_PATH（既定は ./db.sqlite）

cargo run
```

Docker で動かす場合は、環境変数をまとめたファイルを渡します。

```sh
docker build -t udamanami .
docker run --rm --env-file .env \
  -v "$PWD/data:/data" udamanami
```

`entrypoint.sh` は `UDAMANAMI_ENV_SECRET` が未設定なら何もせず、
渡された環境変数のままボットを起動します（GCP 以外でもそのまま動きます）。

## デプロイ

GitHub Actions が `main` への push をトリガーに、イメージをビルドして
GCP Compute Engine（Container-Optimized OS）へロールアウトします。
セットアップ手順と運用は [`deploy/README.md`](deploy/README.md) を参照してください。

## 環境変数一覧

| 変数 | 必須 | 用途 |
| --- | --- | --- |
| `DISCORD_TOKEN` | ✓ | Discord ボットトークン |
| `DISCORD_GUILD_ID` | ✓ | 対象ギルド ID |
| `LLM_API_KEY` | ✓ | LLM の API キー（`OPENAI_API_KEY` でも可） |
| `LLM_MODELS` | | 選べるモデル（カンマ区切り。未設定時は既定の 3 モデル） |
| `LLM_MODEL` | | 既定モデル（未設定時は `LLM_MODELS` の先頭） |
| `LLM_BASE_URL` | | OpenAI 互換の base URL（既定 `https://api.openai.com/v1`） |
| `DEFAULT_CHANNEL_ID` | ✓ | 代筆先が未設定のときの既定の送信先 |
| `DEBUG_ROOM_ID` | | AI 雑談・起動あいさつ用チャンネル |
| `JAIL_MARK_ROLE_ID` | | jail コマンド用ロール |
| `JAIL_MAIN_ROLE_ID` | | jail コマンド用ロール |
| `DISABLED_COMMANDS` | | 無効化するコマンド名（カンマ区切り） |
| `DATABASE_PATH` | | SQLite の保存先（既定 `./db.sqlite`） |
| `COMMIT_HASH` / `COMMIT_DATE` | | バージョン表示用（CI が自動設定） |
