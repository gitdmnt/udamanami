# Deploying udamanami to GCP Compute Engine

udamanami runs as a single container on a **Container-Optimized OS (COS)**
Compute Engine VM, started by a **systemd** service (see `cloud-init.yaml`).
GitHub Actions builds the image, pushes it to **Artifact Registry**, and rolls it
out. Secrets live in **Secret Manager**; the SQLite database lives on the VM's
persistent boot disk.

We deliberately do **not** use gcloud's `create-with-container` /
`update-container`: that "container on a VM" convenience is deprecated by Google.
Running the container ourselves via COS + systemd is the supported path and keeps
us off another dead-end service.

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

The bot process listens on no ports and makes only **outbound** connections (to
Discord and the Gemini API), and this deployment adds no inbound firewall rules of
its own. Note, though, that the VM gets an ephemeral **public IP** for outbound
traffic, and on GCP's `default` network the built-in `default-allow-ssh` rule
leaves tcp:22 reachable from the internet (still gated by SSH keys / OS Login).
See [Notes and alternatives](#notes-and-alternatives) to lock that down.

## What gets created

| Resource | Name (default) | Purpose |
| --- | --- | --- |
| Artifact Registry repo | `udamanami` (asia-northeast1) | stores bot images |
| Runtime service account | `udamanami-vm@<project>` | identity the VM runs as |
| Secret | `udamanami-env` | the bot's `.env`, read at container start |
| CI service account | `udamanami-ci@<project>` | identity GitHub Actions deploys as |
| Workload Identity Pool | `github` | keyless GitHub → GCP trust |
| Compute Engine VM | `udamanami-bot` (asia-northeast1-b, `e2-small`, COS) | runs the bot |

## How a deploy works

1. GitHub Actions builds `…/udamanami:<sha>` and `:latest` and pushes both.
2. It renders `deploy/cloud-init.yaml` with this build's image and the Secret
   Manager resource, and writes the result to the VM's `user-data` metadata.
3. If the VM does not exist yet, it is **created** from `cos-stable` with that
   `user-data`. If it exists, the workflow replaces `user-data` and **resets**
   (reboots) the VM — a reset takes roughly a minute.
4. On every boot, COS re-runs cloud-init, which (re)writes `udamanami.service` and
   starts it; the service pulls the rendered image and runs it.

`/etc` on COS is stateless, so we don't `systemctl enable` — COS re-runs
cloud-init on every boot instead. `udamanami.service` also has `Restart=always`,
so the bot comes back on its own after a crash.

## One-time setup

Prerequisites: [`gcloud`](https://cloud.google.com/sdk/docs/install) installed,
and a GCP project where you have Owner/Editor.

```sh
gcloud auth login
gcloud config set project <PROJECT_ID>
```

### 1. Provision GCP resources

```sh
# APIs, Artifact Registry, runtime SA, and the (empty) secret
PROJECT_ID=<PROJECT_ID> ./deploy/setup-gcp.sh
```

### 2. Store the bot configuration in Secret Manager

```sh
cp deploy/udamanami.env.example deploy/udamanami.env
# edit deploy/udamanami.env — put in the real DISCORD_TOKEN, GEMINI_API_KEY, etc.
gcloud secrets versions add udamanami-env \
  --project <PROJECT_ID> --data-file deploy/udamanami.env
```

`deploy/udamanami.env` is git-ignored — never commit real tokens.

### 3. Set up keyless GitHub → GCP auth

```sh
PROJECT_ID=<PROJECT_ID> GITHUB_REPO=gitdmnt/udamanami ./deploy/setup-github-wif.sh
```

The script prints three values. Set them as **repository variables**
(GitHub → Settings → Secrets and variables → Actions → **Variables**):

- `GCP_PROJECT_ID`
- `GCP_WIF_PROVIDER`
- `GCP_DEPLOY_SA`

### 4. Deploy

Push to `main` (or run the **Deploy to Compute Engine** workflow manually). The
first run **creates** the VM; later runs update its `user-data` and reset it.
Watch it in the Actions tab.

## Everyday operations

**Deploy a new version** — merge to `main`. That's it.

**Change configuration / rotate a token** — add a new secret version, then make
the container re-read it. Re-running the deploy workflow does this; to do it by
hand without a full reset:

```sh
gcloud secrets versions add udamanami-env --project <PROJECT_ID> \
  --data-file deploy/udamanami.env
gcloud compute ssh udamanami-bot --zone asia-northeast1-b \
  --command 'sudo systemctl restart udamanami'
```

**View logs**

```sh
gcloud compute ssh udamanami-bot --zone asia-northeast1-b \
  --command 'docker logs -f udamanami'
# or inspect the service:
gcloud compute ssh udamanami-bot --zone asia-northeast1-b \
  --command 'sudo systemctl status udamanami; journalctl -u udamanami -n 100'
```

**Back up the database**

```sh
gcloud compute ssh udamanami-bot --zone asia-northeast1-b \
  --command 'sudo cp /var/lib/udamanami/db.sqlite /tmp/db.sqlite'
gcloud compute scp udamanami-bot:/tmp/db.sqlite ./db.sqlite.bak \
  --zone asia-northeast1-b
```

The database persists across reboots and deploys (it lives on the boot disk at
`/var/lib/udamanami`). It is **not** retained if the VM and its boot disk are
deleted — take a backup before deleting the instance.

`gcloud compute ssh` works out of the box on the `default` network, which already
permits SSH from your machine. This is administrator access only; the bot itself
opens no ports.

## Configuration knobs

Region, zone, machine type, and names are `env:` values at the top of
`.github/workflows/deploy.yml` and defaults in the `deploy/*.sh` scripts. To run
in the US free tier, set the region to `us-central1` and machine type to
`e2-micro`.

## Notes and alternatives

- **Secrets, not metadata.** Config is fetched from Secret Manager at container
  start by `entrypoint.sh`, using the VM's service account. Nothing sensitive is
  stored in instance metadata. When `UDAMANAMI_ENV_SECRET` is unset the image
  runs with whatever environment you pass it, so it still works locally.
- **Zero-downtime deploys.** The default deploy resets the VM (~1 min of
  downtime). If that ever matters, replace the `reset` step with an
  [IAP-tunnelled](https://cloud.google.com/iap/docs/using-tcp-forwarding) SSH
  call that runs `sudo systemctl restart udamanami` — no reboot, at the cost of a
  little extra IAM/firewall setup.
- **Locking down SSH / dropping the public IP.** The VM's public IP is only for
  outbound traffic, but on the `default` network tcp:22 is reachable from the
  internet (auth still required). To restrict it, tighten or replace the
  `default-allow-ssh` firewall rule (e.g. limit the source range to your IP or to
  the [IAP range](https://cloud.google.com/iap/docs/using-tcp-forwarding)
  `35.235.240.0/20`). To remove the public IP entirely, add `--no-address` to the
  create step and set up [Cloud NAT](https://cloud.google.com/nat/docs/overview)
  for egress.
- **Durable database.** For a database that survives instance deletion, attach a
  separate persistent disk and mount it at `/var/lib/udamanami`, or run a
  scheduled backup of `db.sqlite` to Cloud Storage.
