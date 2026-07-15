#!/usr/bin/env bash
#
# 既存の GCP リソース(setup-gcp.sh / setup-github-wif.sh で作成済みのもの)を
# Terraform state に取り込む。新規プロジェクトに立てる場合は不要で、
# terraform apply だけでよい。
#
# Usage:
#   cd infra && terraform init && PROJECT_ID=manami-502312 ./import.sh
#
set -euo pipefail

PROJECT_ID="${PROJECT_ID:?set PROJECT_ID}"
REGION="${REGION:-us-central1}"
AR_REPO="${AR_REPO:-udamanami}"
SECRET_NAME="${SECRET_NAME:-udamanami-env}"
RUNTIME_SA_ID="${RUNTIME_SA_ID:-udamanami-vm}"
CI_SA_ID="${CI_SA_ID:-udamanami-ci}"
POOL_ID="${POOL_ID:-github}"
PROVIDER_ID="${PROVIDER_ID:-github}"
GITHUB_REPO="${GITHUB_REPO:-gitdmnt/udamanami}"

RUNTIME_SA="${RUNTIME_SA_ID}@${PROJECT_ID}.iam.gserviceaccount.com"
CI_SA="${CI_SA_ID}@${PROJECT_ID}.iam.gserviceaccount.com"
PROJECT_NUMBER="$(gcloud projects describe "${PROJECT_ID}" --format 'value(projectNumber)')"

tf_import() {
  # 取り込み済みならスキップして冪等にする。
  if terraform state show "$1" >/dev/null 2>&1; then
    echo "    already in state: $1"
  else
    terraform import -var "project_id=${PROJECT_ID}" "$1" "$2"
  fi
}

for api in compute artifactregistry secretmanager iam iamcredentials sts; do
  tf_import "google_project_service.apis[\"${api}.googleapis.com\"]" \
    "${PROJECT_ID}/${api}.googleapis.com"
done

tf_import google_artifact_registry_repository.images \
  "projects/${PROJECT_ID}/locations/${REGION}/repositories/${AR_REPO}"

tf_import google_service_account.runtime \
  "projects/${PROJECT_ID}/serviceAccounts/${RUNTIME_SA}"
tf_import google_service_account.ci \
  "projects/${PROJECT_ID}/serviceAccounts/${CI_SA}"

tf_import google_artifact_registry_repository_iam_member.runtime_pull \
  "projects/${PROJECT_ID}/locations/${REGION}/repositories/${AR_REPO} roles/artifactregistry.reader serviceAccount:${RUNTIME_SA}"
tf_import google_artifact_registry_repository_iam_member.ci_push \
  "projects/${PROJECT_ID}/locations/${REGION}/repositories/${AR_REPO} roles/artifactregistry.writer serviceAccount:${CI_SA}"

tf_import google_secret_manager_secret.env \
  "projects/${PROJECT_ID}/secrets/${SECRET_NAME}"
tf_import google_secret_manager_secret_iam_member.runtime_read \
  "projects/${PROJECT_ID}/secrets/${SECRET_NAME} roles/secretmanager.secretAccessor serviceAccount:${RUNTIME_SA}"

tf_import google_iam_workload_identity_pool.github \
  "projects/${PROJECT_ID}/locations/global/workloadIdentityPools/${POOL_ID}"
tf_import google_iam_workload_identity_pool_provider.github \
  "projects/${PROJECT_ID}/locations/global/workloadIdentityPools/${POOL_ID}/providers/${PROVIDER_ID}"

tf_import google_project_iam_member.ci_instance_admin \
  "${PROJECT_ID} roles/compute.instanceAdmin.v1 serviceAccount:${CI_SA}"

tf_import google_service_account_iam_member.ci_act_as_runtime \
  "projects/${PROJECT_ID}/serviceAccounts/${RUNTIME_SA} roles/iam.serviceAccountUser serviceAccount:${CI_SA}"
tf_import google_service_account_iam_member.github_impersonate_ci \
  "projects/${PROJECT_ID}/serviceAccounts/${CI_SA} roles/iam.workloadIdentityUser principalSet://iam.googleapis.com/projects/${PROJECT_NUMBER}/locations/global/workloadIdentityPools/${POOL_ID}/attribute.repository/${GITHUB_REPO}"

echo
echo "==> Import done. Run 'terraform plan' — 差分が出ないことを確認してください。"
