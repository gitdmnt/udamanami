# udamanami の一回きりの GCP セットアップ。
# app/deploy/setup-gcp.sh と setup-github-wif.sh を Terraform に置き換えたもの。
#
# Compute Engine インスタンス本体はここでは管理しない。デプロイのたびに
# GitHub Actions が user-data(cloud-init)を書き換えて reset する運用のため、
# Terraform 管理にすると毎回差分が出て衝突する。インスタンスが存在しなければ
# ワークフローが自己プロビジョニングする(.github/workflows/deploy.yml 参照)。

data "google_project" "this" {
  project_id = var.project_id
}

locals {
  runtime_sa_email = google_service_account.runtime.email
  ci_sa_email      = google_service_account.ci.email
}

# --- APIs -------------------------------------------------------------------

resource "google_project_service" "apis" {
  for_each = toset([
    "compute.googleapis.com",
    "artifactregistry.googleapis.com",
    "secretmanager.googleapis.com",
    "iam.googleapis.com",
    "iamcredentials.googleapis.com",
    "sts.googleapis.com",
  ])

  service            = each.value
  disable_on_destroy = false
}

# --- Artifact Registry ------------------------------------------------------

resource "google_artifact_registry_repository" "images" {
  repository_id = var.ar_repo
  location      = var.region
  format        = "DOCKER"
  description   = "udamanami bot images"

  depends_on = [google_project_service.apis]
}

# --- Runtime SA(VM が使う)-------------------------------------------------

resource "google_service_account" "runtime" {
  account_id   = var.runtime_sa_id
  display_name = "udamanami bot runtime (Compute Engine)"
}

resource "google_artifact_registry_repository_iam_member" "runtime_pull" {
  repository = google_artifact_registry_repository.images.name
  location   = var.region
  role       = "roles/artifactregistry.reader"
  member     = "serviceAccount:${local.runtime_sa_email}"
}

# --- Secret Manager(ボットの .env)-----------------------------------------
# シークレットの「箱」だけ管理する。中身(バージョン)は state に残したくないので
# 従来どおり手動で追加する:
#   gcloud secrets versions add udamanami-env --data-file app/deploy/udamanami.env

resource "google_secret_manager_secret" "env" {
  secret_id = var.secret_name

  replication {
    auto {}
  }

  depends_on = [google_project_service.apis]
}

resource "google_secret_manager_secret_iam_member" "runtime_read" {
  secret_id = google_secret_manager_secret.env.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${local.runtime_sa_email}"
}

# --- Workload Identity Federation(GitHub Actions → GCP、鍵なし)------------

resource "google_iam_workload_identity_pool" "github" {
  workload_identity_pool_id = var.wif_pool_id
  display_name              = "GitHub Actions"

  depends_on = [google_project_service.apis]
}

resource "google_iam_workload_identity_pool_provider" "github" {
  workload_identity_pool_id          = google_iam_workload_identity_pool.github.workload_identity_pool_id
  workload_identity_pool_provider_id = var.wif_provider_id
  display_name                       = "GitHub"

  oidc {
    issuer_uri = "https://token.actions.githubusercontent.com"
  }

  attribute_mapping = {
    "google.subject"       = "assertion.sub"
    "attribute.repository" = "assertion.repository"
    "attribute.ref"        = "assertion.ref"
  }

  attribute_condition = "assertion.repository == '${var.github_repo}'"
}

# --- CI SA(GitHub Actions が impersonate する)------------------------------

resource "google_service_account" "ci" {
  account_id   = var.ci_sa_id
  display_name = "udamanami GitHub Actions deployer"
}

# イメージの push はリポジトリ単位に絞る。
resource "google_artifact_registry_repository_iam_member" "ci_push" {
  repository = google_artifact_registry_repository.images.name
  location   = var.region
  role       = "roles/artifactregistry.writer"
  member     = "serviceAccount:${local.ci_sa_email}"
}

# インスタンスの作成にはプロジェクト/ゾーン単位の権限が要るため、
# instanceAdmin.v1 だけはプロジェクト全体に付与する。ボット専用プロジェクトで
# 運用することで影響範囲をボットに限定する。
resource "google_project_iam_member" "ci_instance_admin" {
  project = var.project_id
  role    = "roles/compute.instanceAdmin.v1"
  member  = "serviceAccount:${local.ci_sa_email}"
}

# CI SA が VM を runtime SA として起動できるようにする。
resource "google_service_account_iam_member" "ci_act_as_runtime" {
  service_account_id = google_service_account.runtime.name
  role               = "roles/iam.serviceAccountUser"
  member             = "serviceAccount:${local.ci_sa_email}"
}

# GitHub リポジトリからの OIDC トークンで CI SA を impersonate できるようにする。
resource "google_service_account_iam_member" "github_impersonate_ci" {
  service_account_id = google_service_account.ci.name
  role               = "roles/iam.workloadIdentityUser"
  member             = "principalSet://iam.googleapis.com/${google_iam_workload_identity_pool.github.name}/attribute.repository/${var.github_repo}"
}
