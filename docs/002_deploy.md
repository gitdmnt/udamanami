## 概要

うだまなみは現状以下の手順でGoogle Compute Engine上にデプロイされる。

1. GitHub ActionsによってDockerイメージがビルドされる。
2. Artifact Registryにpushされる。
3. Compute Engine VM上に展開される。
4. systemdによって起動される (deploy/cloud-init.yamlを参照)。

secretsはsecret managerに置かれ、SQLiteのDBはVMのストレージに置かれる。

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
