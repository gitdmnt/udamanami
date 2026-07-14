## 概要

うだまなみは現状以下の手順でGoogle Compute Engine上にデプロイされる。

1. GitHub ActionsによってDockerイメージがビルドされる。
2. Artifact Registryにpushされる。
3. Compute Engine VM上に展開される。
4. systemdによって起動される (deploy/cloud-init.yamlを参照)。

secretsはsecret managerに置かれ、SQLiteのDBはVMのストレージに置かれる。

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

## 日々の運用

新しいバージョンのデプロイは、`main` にマージするだけです。

設定変更やトークンのローテーションは、シークレットの新しいバージョンを追加し、コンテナに読み直させます。
デプロイワークフローを再実行すればこれが行われます。
reset せずに手作業で行うには次のようにします。

```sh
gcloud secrets versions add udamanami-env --project manami-502312 \
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
