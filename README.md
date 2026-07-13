# udamanami

Discord ボット「うだまなみ」の実装です。

serenity でメッセージとスラッシュコマンドを処理し、雑談には Gemini を、
ログや変数の保存には SQLite（sea-orm）を使います。
Discord への外向き接続だけで動き、受信ポートは開けません。

## ローカルでの実行

必要なツールは Rust（stable）です。
設定はすべて環境変数で渡します（`.env` の自動読み込みはしません）。

```sh
export DISCORD_TOKEN=...       # 必須
export GEMINI_API_KEY=...      # 必須
export DISCORD_GUILD_ID=...    # 必須
export ROOMS_ID=...,...        # 反応するチャンネル ID（カンマ区切り）
export DEBUG_ROOM_ID=...       # AI 雑談・起動あいさつを流すチャンネル
# 任意: JAIL_MARK_ROLE_ID, JAIL_MAIN_ROLE_ID, DISABLED_COMMANDS
# 任意: DATABASE_PATH（既定は ./db.sqlite）

cargo run
```

Docker で動かす場合は、環境変数をまとめたファイルを渡します。

```sh
docker build -t udamanami .
docker run --rm --env-file deploy/udamanami.env \
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
| `GEMINI_API_KEY` | ✓ | Gemini API キー |
| `DISCORD_GUILD_ID` | ✓ | 対象ギルド ID |
| `ROOMS_ID` | | 反応するチャンネル ID（カンマ区切り） |
| `DEBUG_ROOM_ID` | | AI 雑談・起動あいさつ用チャンネル |
| `JAIL_MARK_ROLE_ID` | | jail コマンド用ロール |
| `JAIL_MAIN_ROLE_ID` | | jail コマンド用ロール |
| `DISABLED_COMMANDS` | | 無効化するコマンド名（カンマ区切り） |
| `DATABASE_PATH` | | SQLite の保存先（既定 `./db.sqlite`） |
| `COMMIT_HASH` / `COMMIT_DATE` | | バージョン表示用（CI が自動設定） |
