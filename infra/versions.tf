terraform {
  required_version = ">= 1.5"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 6.0"
    }
  }

  # state は GCS で管理(バージョニング有効、public access 禁止)。
  backend "gcs" {
    bucket = "manami-502312-tfstate"
    prefix = "udamanami"
  }
}

provider "google" {
  project = var.project_id
  region  = var.region
}
