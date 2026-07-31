## 概要

このドキュメントは、うだまなみの構成要素と、それぞれのログの見方をまとめる。
障害調査の起点として使う。

## アーキテクチャ

- bot本体: GCE(Compute Engine)VM上でDockerコンテナとして動く Rust/serenity アプリ
- データAPI: Cloudflare Worker(`udamanami-db-api`)
  - D1: 各種データ
  - Vectorize: 記憶のベクトル表現 (1536次元, cosine)
- OpenAI API
  - LLM (GPT系)
  - Embedding (text-embedding-3-small)

botはAPIをRESTで叩く。AUTH_TOKENをBearerで渡して認証をしている。

bot本体のenv(`WORKERS_API_URL` / `WORKERS_API_TOKEN` など)は Secret Manager の `udamanami-env` にある。
Worker側のsecret(`OPENAI_API_KEY` / `AUTH_TOKEN`)は Cloudflare 側に `wrangler secret` で置く。

## ログの見方

### bot本体(GCE)

コンテナ名は `udamanami`。
systemd unit ではなく docker コンテナなので、`journalctl -u udamanami` では取れないことがある。
まずコンテナの有無を確認する。

```sh
gcloud compute ssh udamanami-bot --zone us-central1-a --command 'sudo docker ps -a'
gcloud compute ssh udamanami-bot --zone us-central1-a --command 'sudo docker logs udamanami --tail 200'
# 追尾する場合:
gcloud compute ssh udamanami-bot --zone us-central1-a --command 'sudo docker logs -f udamanami'
```

`docker logs` は現在のコンテナの分しか残らない。
デプロイでコンテナが作り直されると、それ以前のログは消える。

ツール呼び出しの失敗ログはWorkersの方のログを見るべきときもある。

### まなみの応答方針(監査ログ)

雑談の応答は2段で生成される。
第1段(プランナー)が何をどう言うかを構造化データで決め、第2段(演者)がそれをまなみの言い回しに写す。
第1段の出力そのものは Discord に出ないので、`manami::plan` ターゲットのログで追う。

```sh
gcloud compute ssh udamanami-bot --zone us-central1-a --command \
  'sudo docker logs udamanami --tail 500' | grep manami::plan
```

`should_reply=false` の行は「まなみが黙ると決めた」ことを示す。
発言が無いのに行が出ていれば正常な沈黙で、行そのものが無ければ確率のゲート(`/allowreply`)で弾かれたか、そもそも第1段まで到達していない。

`planner failed; falling back` は第1段が使えず退避の方針で応答したことを示す。
`error` フィールドにモデルが返した文字列そのものが入るので、スキーマ違反なのか `LLM_BASE_URL` 側が `text.format` を無視したのかを切り分けられる。

`docker logs` は現在のコンテナぶんしか残らないため、過去に遡っての監査はできない。

### データAPI(Cloudflare Worker)

ランタイムのエラーや例外はこちらに出る。
`wrangler tail` でライブに追う(過去ログは残らない点に注意)。

```sh
cd workers
wrangler tail --format json         # 別端末で流しておき、実際に操作して再現させる
```

エンドポイント単位の切り分けは、読み取り系を直接叩くのが速い(副作用がない)。

```sh
# WORKERS_API_URL / WORKERS_API_TOKEN は Secret Manager の udamanami-env にある
curl -s "$URL/memory/list"  -H "Authorization: Bearer $TOK"          # 200なら D1 読取OK
curl -s "$URL/memory/search?q=test&limit=3" -H "Authorization: Bearer $TOK"  # 200なら embed+Vectorize検索OK
```

`/memory/search` が通れば、OpenAI鍵と Vectorize クエリは生きている。
書き込み系(`POST /memory`)だけが落ちる場合、原因は D1 への INSERT か Vectorize への upsert に絞れる。
`POST /memory` は本番storeへの書き込みなので、再現テストは慎重に行う。

### D1 の中身

読み取り専用でスキーマや件数を確認できる。

```sh
cd workers
wrangler d1 execute udamanami --remote --command \
  "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name" --json
wrangler d1 migrations list udamanami --remote
```

`--json` の出力は先頭が長いので、`tail` で切ると配列の頭が欠けて誤読しやすい。
全体をパースして読むこと。
