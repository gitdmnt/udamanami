#!/usr/bin/env bash
#
# One-time setup of keyless GitHub Actions -> GCP authentication using Workload
# Identity Federation, plus the CI service account the deploy workflow runs as.
#
# Creates:
#   * a Workload Identity Pool + GitHub OIDC provider, restricted to this repo
#   * a CI service account ("udamanami-ci") allowed to push images and manage the
#     bot's Compute Engine instance
#   * the IAM bindings that let the GitHub repo impersonate that SA without any
#     long-lived key
#
# Prerequisites: gcloud installed, logged in as a project admin, and
# ./deploy/setup-gcp.sh already run (it creates the runtime SA referenced here).
#
# Usage:
#   PROJECT_ID=my-project GITHUB_REPO=gitdmnt/udamanami ./deploy/setup-github-wif.sh
#
set -euo pipefail

PROJECT_ID="${PROJECT_ID:?set PROJECT_ID to your GCP project id}"
GITHUB_REPO="${GITHUB_REPO:-gitdmnt/udamanami}" # owner/repo
REGION="${REGION:-asia-northeast1}"
AR_REPO="${AR_REPO:-udamanami}"
POOL_ID="${POOL_ID:-github}"
PROVIDER_ID="${PROVIDER_ID:-github}"
CI_SA_ID="${CI_SA_ID:-udamanami-ci}"
RUNTIME_SA_ID="${RUNTIME_SA_ID:-udamanami-vm}"

CI_SA_EMAIL="${CI_SA_ID}@${PROJECT_ID}.iam.gserviceaccount.com"
RUNTIME_SA_EMAIL="${RUNTIME_SA_ID}@${PROJECT_ID}.iam.gserviceaccount.com"

PROJECT_NUMBER="$(gcloud projects describe "${PROJECT_ID}" \
  --format 'value(projectNumber)')"

echo "==> Project        : ${PROJECT_ID} (number ${PROJECT_NUMBER})"
echo "==> GitHub repo    : ${GITHUB_REPO}"
echo "==> CI SA          : ${CI_SA_EMAIL}"
echo

echo "==> Enabling required APIs..."
gcloud services enable \
  iam.googleapis.com \
  iamcredentials.googleapis.com \
  sts.googleapis.com \
  --project "${PROJECT_ID}"

echo "==> Creating Workload Identity Pool (if missing)..."
if ! gcloud iam workload-identity-pools describe "${POOL_ID}" \
      --project "${PROJECT_ID}" --location global >/dev/null 2>&1; then
  gcloud iam workload-identity-pools create "${POOL_ID}" \
    --project "${PROJECT_ID}" \
    --location global \
    --display-name "GitHub Actions"
else
  echo "    already exists."
fi

echo "==> Creating GitHub OIDC provider (if missing)..."
if ! gcloud iam workload-identity-pools providers describe "${PROVIDER_ID}" \
      --project "${PROJECT_ID}" --location global \
      --workload-identity-pool "${POOL_ID}" >/dev/null 2>&1; then
  gcloud iam workload-identity-pools providers create-oidc "${PROVIDER_ID}" \
    --project "${PROJECT_ID}" \
    --location global \
    --workload-identity-pool "${POOL_ID}" \
    --display-name "GitHub" \
    --issuer-uri "https://token.actions.githubusercontent.com" \
    --attribute-mapping "google.subject=assertion.sub,attribute.repository=assertion.repository,attribute.ref=assertion.ref" \
    --attribute-condition "assertion.repository == '${GITHUB_REPO}'"
else
  echo "    already exists."
fi

echo "==> Creating CI service account (if missing)..."
if ! gcloud iam service-accounts describe "${CI_SA_EMAIL}" \
      --project "${PROJECT_ID}" >/dev/null 2>&1; then
  gcloud iam service-accounts create "${CI_SA_ID}" \
    --project "${PROJECT_ID}" \
    --display-name "udamanami GitHub Actions deployer"
else
  echo "    already exists."
fi

echo "==> Granting the CI SA push access to the '${AR_REPO}' Artifact Registry repo..."
# Scoped to the single repo rather than project-wide artifactregistry.writer.
gcloud artifacts repositories add-iam-policy-binding "${AR_REPO}" \
  --project "${PROJECT_ID}" \
  --location "${REGION}" \
  --member "serviceAccount:${CI_SA_EMAIL}" \
  --role roles/artifactregistry.writer \
  --condition None >/dev/null

echo "==> Granting the CI SA permission to manage the bot instance..."
# roles/compute.instanceAdmin.v1 is granted project-wide: creating an instance
# needs project/zone-scoped permissions that cannot be narrowed to a single,
# not-yet-existing instance. Run the bot in a dedicated GCP project so the blast
# radius of this grant is just the bot.
gcloud projects add-iam-policy-binding "${PROJECT_ID}" \
  --member "serviceAccount:${CI_SA_EMAIL}" \
  --role roles/compute.instanceAdmin.v1 \
  --condition None >/dev/null

echo "==> Allowing the CI SA to run the instance as the runtime SA..."
gcloud iam service-accounts add-iam-policy-binding "${RUNTIME_SA_EMAIL}" \
  --project "${PROJECT_ID}" \
  --member "serviceAccount:${CI_SA_EMAIL}" \
  --role roles/iam.serviceAccountUser \
  --condition None >/dev/null

echo "==> Letting the GitHub repo impersonate the CI SA (keyless)..."
gcloud iam service-accounts add-iam-policy-binding "${CI_SA_EMAIL}" \
  --project "${PROJECT_ID}" \
  --role roles/iam.workloadIdentityUser \
  --member "principalSet://iam.googleapis.com/projects/${PROJECT_NUMBER}/locations/global/workloadIdentityPools/${POOL_ID}/attribute.repository/${GITHUB_REPO}" \
  --condition None >/dev/null

PROVIDER_RESOURCE="projects/${PROJECT_NUMBER}/locations/global/workloadIdentityPools/${POOL_ID}/providers/${PROVIDER_ID}"

cat <<EOF

==> Done. Set these as repository variables on GitHub
    (Settings > Secrets and variables > Actions > Variables):

      GCP_PROJECT_ID   = ${PROJECT_ID}
      GCP_WIF_PROVIDER = ${PROVIDER_RESOURCE}
      GCP_DEPLOY_SA    = ${CI_SA_EMAIL}

    You can also set them with the gh CLI:

      gh variable set GCP_PROJECT_ID   --body '${PROJECT_ID}'
      gh variable set GCP_WIF_PROVIDER --body '${PROVIDER_RESOURCE}'
      gh variable set GCP_DEPLOY_SA    --body '${CI_SA_EMAIL}'
EOF
