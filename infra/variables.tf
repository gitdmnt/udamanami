variable "project_id" {
  description = "GCP project id"
  type        = string
}

variable "region" {
  description = "Artifact Registry などのリージョン(Always Free 対象の US リージョン)"
  type        = string
  default     = "us-central1"
}

variable "github_repo" {
  description = "デプロイを許可する GitHub リポジトリ (owner/repo)"
  type        = string
  default     = "gitdmnt/udamanami"
}

variable "ar_repo" {
  description = "Artifact Registry リポジトリ名"
  type        = string
  default     = "udamanami"
}

variable "secret_name" {
  description = "ボットの .env を保持する Secret Manager シークレット名"
  type        = string
  default     = "udamanami-env"
}

variable "runtime_sa_id" {
  description = "VM が実行に使うサービスアカウント ID"
  type        = string
  default     = "udamanami-vm"
}

variable "ci_sa_id" {
  description = "GitHub Actions が impersonate するサービスアカウント ID"
  type        = string
  default     = "udamanami-ci"
}

variable "wif_pool_id" {
  description = "Workload Identity Pool ID"
  type        = string
  default     = "github"
}

variable "wif_provider_id" {
  description = "Workload Identity Pool 内の OIDC プロバイダ ID"
  type        = string
  default     = "github"
}
