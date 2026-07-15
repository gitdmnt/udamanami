# syntax=docker/dockerfile:1

FROM lukemathwalker/cargo-chef:latest-rust-1 AS chef
WORKDIR /app

FROM chef AS planner
COPY . .
RUN cargo chef prepare --recipe-path recipe.json

FROM chef AS builder
COPY --from=planner /app/recipe.json recipe.json
RUN cargo chef cook --release --recipe-path recipe.json

COPY . .
RUN cargo build --release

FROM debian:trixie-slim AS runtime

# Runtime dependencies:
#   ca-certificates, libssl3 -> TLS for the Gemini API (reqwest + native-tls)
#   curl, jq                 -> entrypoint fetches secrets from Secret Manager on GCP
#   tini                     -> proper PID 1: reaps zombies and forwards SIGTERM
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        libssl3 \
        curl \
        jq \
        tini \
    && rm -rf /var/lib/apt/lists/*

# Version metadata, shown as the bot's Discord presence. Injected by CI at build
# time; can be overridden at run time via the environment.
ARG COMMIT_HASH=""
ARG COMMIT_DATE=""
ENV COMMIT_HASH=${COMMIT_HASH} \
    COMMIT_DATE=${COMMIT_DATE} \
    DATABASE_PATH=/data/db.sqlite

# /data is a mount point for the persistent SQLite database on Compute Engine,
# and a throwaway directory when the image is run locally without a volume.
RUN mkdir -p /data

COPY --from=builder /app/target/release/udamanami /usr/local/bin/udamanami
COPY app/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

ENTRYPOINT ["/usr/bin/tini", "--", "/usr/local/bin/entrypoint.sh"]
CMD ["udamanami"]
