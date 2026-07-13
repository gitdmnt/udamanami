#!/usr/bin/env bash
# Container entrypoint for the udamanami Discord bot.
#
# On GCP Compute Engine we keep secrets in Secret Manager rather than baking them
# into the image or exposing them through instance metadata. When the environment
# variable UDAMANAMI_ENV_SECRET points at a secret version, this script fetches it
# using the instance's service account (Application Default Credentials served by
# the metadata server), loads it into the environment, then execs the bot.
#
# When UDAMANAMI_ENV_SECRET is unset -- e.g. a local `docker run --env-file ...` --
# it does nothing special and simply execs the bot with the ambient environment,
# so the image stays portable and runnable off GCP.
set -euo pipefail

log() { printf '[entrypoint] %s\n' "$*" >&2; }

# Fetch a Secret Manager secret payload and load "KEY=value" lines into the env.
load_env_from_secret_manager() {
    local secret="$1"
    # Use the metadata server's stable link-local IP rather than the
    # metadata.google.internal hostname: DNS for that name is not guaranteed
    # inside a container's network namespace, but the IP always is.
    local metadata="http://169.254.169.254/computeMetadata/v1"
    local token payload env_file

    log "loading environment from Secret Manager: ${secret}"

    token="$(curl -sf -H 'Metadata-Flavor: Google' \
        "${metadata}/instance/service-accounts/default/token" \
        | jq -r '.access_token')" || {
        log "ERROR: could not obtain an access token from the metadata server"
        return 1
    }

    payload="$(curl -sf -H "Authorization: Bearer ${token}" \
        "https://secretmanager.googleapis.com/v1/${secret}:access" \
        | jq -r '.payload.data')" || {
        log "ERROR: could not access secret ${secret}"
        return 1
    }

    env_file="$(mktemp)"
    # shellcheck disable=SC2064
    trap "rm -f '${env_file}'" RETURN
    # Secret Manager returns standard base64; strip any stray CRs from the payload.
    printf '%s' "${payload}" | base64 -d | tr -d '\r' > "${env_file}"

    set -a
    # shellcheck source=/dev/null
    . "${env_file}"
    set +a
}

if [[ -n "${UDAMANAMI_ENV_SECRET:-}" ]]; then
    load_env_from_secret_manager "${UDAMANAMI_ENV_SECRET}"
else
    log "UDAMANAMI_ENV_SECRET is not set; using the ambient environment"
fi

exec "$@"
