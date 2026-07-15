# infra

udamanami の GCP 側セットアップの Terraform 構成。
従来の `app/deploy/setup-gcp.sh` と `app/deploy/setup-github-wif.sh` を置き換える。

## 管理対象

- 必要な API の有効化
- Artifact Registry リポジトリ(`udamanami`)
- runtime SA(`udamanami-vm`)と image pull / secret read の権限
- Secret Manager シークレット `udamanami-env` の箱(中身は手動で `gcloud secrets versions add`)
- Workload Identity Pool + GitHub OIDC プロバイダ
- CI SA(`udamanami-ci`)と各種 IAM バインディング

## 管理対象外(意図的)

- **Compute Engine インスタンス** — デプロイごとに GitHub Actions が
  user-data を書き換えて reset する運用のため、Terraform 管理にすると衝突する。
  インスタンスがなければワークフローが自動で作成する。
- **シークレットの中身** — state ファイルに平文で残るため。
- **Cloudflare 側(Workers / D1 / Vectorize)** — wrangler.toml が管理しており、
  Vectorize は Terraform provider 未対応。

## 使い方

state は GCS backend(`gs://manami-502312-tfstate/udamanami/`、バージョニング有効)で管理している。

```sh
cd infra
cp terraform.tfvars.example terraform.tfvars   # project_id を設定
export GOOGLE_OAUTH_ACCESS_TOKEN="$(gcloud auth print-access-token)"  # ADC が別アカウントの場合
terraform init
```

既存環境を取り込む場合(現在の manami-502312 はこちら):

```sh
PROJECT_ID=manami-502312 ./import.sh
terraform plan   # 差分ゼロを確認
```

新規プロジェクトに立てる場合は `terraform apply` のみでよい。

apply 後、GitHub リポジトリ変数を設定する:

```sh
gh variable set GCP_PROJECT_ID   --body "$(terraform output -raw gcp_project_id)"
gh variable set GCP_WIF_PROVIDER --body "$(terraform output -raw gcp_wif_provider)"
gh variable set GCP_DEPLOY_SA    --body "$(terraform output -raw gcp_deploy_sa)"
```

シークレットの中身は従来どおり:

```sh
gcloud secrets versions add udamanami-env \
  --project <PROJECT_ID> --data-file app/deploy/udamanami.env
```
