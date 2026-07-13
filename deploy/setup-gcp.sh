#!/usr/bin/env bash
#
# One-time GCP setup for deploying the udamanami Discord bot to Compute Engine.
#
# Creates:
#   * the required APIs
#   * an Artifact Registry Docker repository
#   * the runtime service account the VM runs as, with permission to pull images
#     and read the bot's secret
#   * a Secret Manager secret ("udamanami-env") holding the bot's .env
#
# The Compute Engine instance itself is created by the GitHub Actions deploy
# workflow on its first run (it self-provisions if the instance is missing).
#
# Prerequisites: gcloud is installed and you are logged in with a principal that
# can administer the project (Owner or Editor). Run:
#   gcloud auth login
#   gcloud config set project <PROJECT_ID>
#
# Usage:
#   PROJECT_ID=my-project ./deploy/setup-gcp.sh
#   PROJECT_ID=my-project ENV_FILE=deploy/udamanami.env ./deploy/setup-gcp.sh
#
set -euo pipefail

PROJECT_ID="${PROJECT_ID:?set PROJECT_ID to your GCP project id}"
REGION="${REGION:-asia-northeast1}"
AR_REPO="${AR_REPO:-udamanami}"
RUNTIME_SA_ID="${RUNTIME_SA_ID:-udamanami-vm}"
SECRET_NAME="${SECRET_NAME:-udamanami-env}"
ENV_FILE="${ENV_FILE:-}"

RUNTIME_SA_EMAIL="${RUNTIME_SA_ID}@${PROJECT_ID}.iam.gserviceaccount.com"

echo "==> Project        : ${PROJECT_ID}"
echo "==> Region         : ${REGION}"
echo "==> Artifact repo  : ${AR_REPO}"
echo "==> Runtime SA     : ${RUNTIME_SA_EMAIL}"
echo "==> Secret         : ${SECRET_NAME}"
echo

echo "==> Enabling required APIs..."
gcloud services enable \
  compute.googleapis.com \
  artifactregistry.googleapis.com \
  secretmanager.googleapis.com \
  --project "${PROJECT_ID}"

echo "==> Creating Artifact Registry repository (if missing)..."
if ! gcloud artifacts repositories describe "${AR_REPO}" \
      --project "${PROJECT_ID}" --location "${REGION}" >/dev/null 2>&1; then
  gcloud artifacts repositories create "${AR_REPO}" \
    --project "${PROJECT_ID}" \
    --location "${REGION}" \
    --repository-format docker \
    --description "udamanami bot images"
else
  echo "    already exists."
fi

echo "==> Creating runtime service account (if missing)..."
if ! gcloud iam service-accounts describe "${RUNTIME_SA_EMAIL}" \
      --project "${PROJECT_ID}" >/dev/null 2>&1; then
  gcloud iam service-accounts create "${RUNTIME_SA_ID}" \
    --project "${PROJECT_ID}" \
    --display-name "udamanami bot runtime (Compute Engine)"
else
  echo "    already exists."
fi

echo "==> Granting the runtime SA image-pull and secret-access permissions..."
# Pull images from Artifact Registry.
gcloud artifacts repositories add-iam-policy-binding "${AR_REPO}" \
  --project "${PROJECT_ID}" \
  --location "${REGION}" \
  --member "serviceAccount:${RUNTIME_SA_EMAIL}" \
  --role roles/artifactregistry.reader \
  --condition None >/dev/null

echo "==> Creating the Secret Manager secret (if missing)..."
if ! gcloud secrets describe "${SECRET_NAME}" \
      --project "${PROJECT_ID}" >/dev/null 2>&1; then
  gcloud secrets create "${SECRET_NAME}" \
    --project "${PROJECT_ID}" \
    --replication-policy automatic
else
  echo "    already exists."
fi

# Let only the runtime SA read the secret.
gcloud secrets add-iam-policy-binding "${SECRET_NAME}" \
  --project "${PROJECT_ID}" \
  --member "serviceAccount:${RUNTIME_SA_EMAIL}" \
  --role roles/secretmanager.secretAccessor \
  --condition None >/dev/null

if [[ -n "${ENV_FILE}" ]]; then
  if [[ ! -f "${ENV_FILE}" ]]; then
    echo "ERROR: ENV_FILE '${ENV_FILE}' not found." >&2
    exit 1
  fi
  echo "==> Adding a new secret version from ${ENV_FILE}..."
  gcloud secrets versions add "${SECRET_NAME}" \
    --project "${PROJECT_ID}" \
    --data-file "${ENV_FILE}"
else
  echo
  echo "==> No ENV_FILE given. Push the bot's configuration before deploying:"
  echo "      cp deploy/udamanami.env.example deploy/udamanami.env"
  echo "      # edit deploy/udamanami.env with your real tokens"
  echo "      gcloud secrets versions add ${SECRET_NAME} \\"
  echo "        --project ${PROJECT_ID} --data-file deploy/udamanami.env"
fi

echo
echo "==> Done. Next: run ./deploy/setup-github-wif.sh, then push to main."
