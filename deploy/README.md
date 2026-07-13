# udamanami の GCP Compute Engine デプロイ

udamanami は Container-Optimized OS（COS）の Compute Engine VM 上で、1 つのコンテナとして動きます。
起動は systemd サービスが担います（`cloud-init.yaml` を参照）。
GitHub Actions がイメージをビルドし、Artifact Registry へ push して、VM へロールアウトします。
秘密情報は Secret Manager に置き、SQLite データベースは VM の永続ブートディスクに置きます。

gcloud の `create-with-container` と `update-container`（コンテナを VM に載せる簡便機能）は使いません。
これらは Google が非推奨としているためです。
COS と systemd で自前でコンテナを動かす方法が公式に支持された経路であり、行き止まりのサービスに再び乗らずに済みます。

```
GitHub push to main
   │  (Workload Identity Federation — keyless)
   ▼
GitHub Actions ── build ──▶ Artifact Registry
   │                              ▲
   │ render user-data             │ docker pull (VM service account)
   │ + reset the VM               │
   ▼                              │
Compute Engine VM (COS) ── systemd udamanami.service ──▶ container
                                        │ entrypoint reads
                                        ▼
                                 Secret Manager (udamanami-env)
```

ボットのプロセスはどのポートも listen せず、外向きの接続だけを行います（Discord と LLM API）。
このデプロイ自体は受信ファイアウォールルールを一切追加しません。
ただし VM は外向き通信のためにエフェメラルな公開 IP を持ち、GCP の `default` ネットワークでは組み込みの `default-allow-ssh` ルールによって tcp:22 がインターネットから到達可能です（アクセスには SSH 鍵や OS Login が必要）。
締め出す方法は「補足と代替案」を参照してください。

## 作成されるリソース

| リソース | 名前（既定） | 役割 |
| --- | --- | --- |
| Artifact Registry リポジトリ | `udamanami`（us-central1） | ボットのイメージを保管 |
| ランタイム用サービスアカウント | `udamanami-vm@<project>` | VM が名乗る ID |
| シークレット | `udamanami-env` | ボットの `.env`。コンテナ起動時に読む |
| CI 用サービスアカウント | `udamanami-ci@<project>` | GitHub Actions が名乗る ID |
| Workload Identity プール | `github` | GitHub から GCP へのキーレス信頼 |
| Compute Engine VM | `udamanami-bot`（us-central1-a, `e2-micro`, COS） | ボットを実行 |

## デプロイの流れ

1. GitHub Actions が `…/udamanami:<sha>` と `:latest` をビルドし、両方を push します。
2. `deploy/cloud-init.yaml` に、そのビルドのイメージと Secret Manager のリソース名を差し込み、結果を VM の `user-data` メタデータへ書き込みます。
3. VM がまだ無ければ、その `user-data` を付けて `cos-stable` から作成します。既にあれば、`user-data` を置き換えて VM を reset（再起動）します。reset には 1 分ほどかかります。
4. 起動のたびに COS が cloud-init を再実行し、`udamanami.service` を書き（直し）て起動します。サービスは差し込まれたイメージを pull して実行します。

COS の `/etc` は揮発性なので、`systemctl enable` はしません。
代わりに COS が起動のたびに cloud-init を再実行します。
`udamanami.service` は `Restart=always` なので、クラッシュしてもボットは自力で復帰します。

## 初回セットアップ

前提は、[`gcloud`](https://cloud.google.com/sdk/docs/install) がインストール済みで、Owner か Editor 権限を持つ GCP プロジェクトがあることです。

```sh
gcloud auth login
gcloud config set project <PROJECT_ID>
```

### 1. GCP リソースを用意する

```sh
# API、Artifact Registry、ランタイム SA、（空の）シークレットを作成
PROJECT_ID=<PROJECT_ID> ./deploy/setup-gcp.sh
```

### 2. ボットの設定を Secret Manager に保存する

```sh
cp deploy/udamanami.env.example deploy/udamanami.env
# deploy/udamanami.env を編集し、実際の DISCORD_TOKEN や LLM_API_KEY などを入れる
gcloud secrets versions add udamanami-env \
  --project <PROJECT_ID> --data-file deploy/udamanami.env
```

`deploy/udamanami.env` は git 管理外です。
実トークンをコミットしないでください。
`/model` と `/auto` で選べるモデルは `LLM_MODELS`（カンマ区切り）で決まります。
LLM エンドポイントは既定で OpenAI を指し、`LLM_BASE_URL` で切り替えられます。

### 3. GitHub から GCP へのキーレス認証を設定する

```sh
PROJECT_ID=<PROJECT_ID> GITHUB_REPO=gitdmnt/udamanami ./deploy/setup-github-wif.sh
```

スクリプトが 3 つの値を出力します。
これらを**リポジトリ変数**として設定してください（GitHub → Settings → Secrets and variables → Actions → Variables）。

- `GCP_PROJECT_ID`
- `GCP_WIF_PROVIDER`
- `GCP_DEPLOY_SA`

### 4. デプロイする

`main` に push します（または Deploy to Compute Engine ワークフローを手動実行します）。
初回の実行で VM を作成し、以降の実行は `user-data` を更新して reset します。
進行状況は Actions タブで確認できます。

## 日々の運用

新しいバージョンのデプロイは、`main` にマージするだけです。

設定変更やトークンのローテーションは、シークレットの新しいバージョンを追加し、コンテナに読み直させます。
デプロイワークフローを再実行すればこれが行われます。
reset せずに手作業で行うには次のようにします。

```sh
gcloud secrets versions add udamanami-env --project <PROJECT_ID> \
  --data-file deploy/udamanami.env
gcloud compute ssh udamanami-bot --zone us-central1-a \
  --command 'sudo systemctl restart udamanami'
```

ログを見るには次のようにします。

```sh
gcloud compute ssh udamanami-bot --zone us-central1-a \
  --command 'docker logs -f udamanami'
# またはサービスの状態を調べる:
gcloud compute ssh udamanami-bot --zone us-central1-a \
  --command 'sudo systemctl status udamanami; journalctl -u udamanami -n 100'
```

データベースをバックアップするには次のようにします。

```sh
gcloud compute ssh udamanami-bot --zone us-central1-a \
  --command 'sudo cp /var/lib/udamanami/db.sqlite /tmp/db.sqlite'
gcloud compute scp udamanami-bot:/tmp/db.sqlite ./db.sqlite.bak \
  --zone us-central1-a
```

データベースは再起動やデプロイをまたいで残ります（ブートディスク上の `/var/lib/udamanami` にあります）。
VM とそのブートディスクを削除すると残りません。
インスタンスを削除する前にバックアップを取ってください。

`gcloud compute ssh` は `default` ネットワークでそのまま使えます。
このネットワークは手元のマシンからの SSH を既に許可しているためです。
これは管理者アクセス専用であり、ボット自体はどのポートも開けません。

## 設定値の変更

リージョン、ゾーン、マシンタイプ、各種名前は、`.github/workflows/deploy.yml` 冒頭の `env:` の値と、`deploy/*.sh` スクリプトの既定値にあります。
既定値は無料枠（Always Free）向けにしてあります（リージョン `us-central1`、マシンタイプ `e2-micro`、`pd-standard` の 30GB ブートディスク）。
リージョンは無料枠対象の `us-west1` や `us-east1` にも変えられます。
無料枠は 1 台のみ、かつ非プリエンプティブが条件です。
ただし外部 IPv4 アドレスは無料枠に含まれず、月 2 ドル前後かかります。
これはボットが外部へ接続するために必要で、外すと egress に Cloud NAT が要り、かえって割高になります。

## 補足と代替案

- **秘密情報はメタデータに置かない**：設定はコンテナ起動時に `entrypoint.sh` が VM のサービスアカウントで Secret Manager から取得します。機密はインスタンスメタデータに一切保存しません。`UDAMANAMI_ENV_SECRET` が未設定なら、イメージは渡された環境変数のまま動くので、ローカルでもそのまま動作します。
- **無停止デプロイ**：既定のデプロイは VM を reset するため、1 分ほど停止します。これが問題になる場合は、`reset` の代わりに [IAP トンネル](https://cloud.google.com/iap/docs/using-tcp-forwarding)経由の SSH で `sudo systemctl restart udamanami` を実行します。再起動は不要になりますが、IAM とファイアウォールの設定が少し増えます。
- **SSH の締め出しと公開 IP の除去**：VM の公開 IP は外向き通信のためだけにありますが、`default` ネットワークでは tcp:22 がインターネットから到達可能です（認証は必要）。制限するには、`default-allow-ssh` ファイアウォールルールを厳しくするか置き換えます（送信元範囲を自分の IP や [IAP レンジ](https://cloud.google.com/iap/docs/using-tcp-forwarding) `35.235.240.0/20` に絞る、など）。公開 IP を完全に無くすには、作成ステップに `--no-address` を足し、外向き通信のために [Cloud NAT](https://cloud.google.com/nat/docs/overview) を設定します。
- **データベースの永続化を強くする**：インスタンス削除後も残るデータベースにするには、別の永続ディスクを付けて `/var/lib/udamanami` にマウントするか、`db.sqlite` を Cloud Storage へ定期バックアップします。
