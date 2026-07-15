# GitHub リポジトリ変数(Settings > Secrets and variables > Actions > Variables)
# に設定する値。gh CLI なら:
#   gh variable set GCP_PROJECT_ID   --body "$(terraform output -raw gcp_project_id)"
#   gh variable set GCP_WIF_PROVIDER --body "$(terraform output -raw gcp_wif_provider)"
#   gh variable set GCP_DEPLOY_SA    --body "$(terraform output -raw gcp_deploy_sa)"

output "gcp_project_id" {
  value = var.project_id
}

output "gcp_wif_provider" {
  value = google_iam_workload_identity_pool_provider.github.name
}

output "gcp_deploy_sa" {
  value = google_service_account.ci.email
}
