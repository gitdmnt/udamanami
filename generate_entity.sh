#!/bin/sh

# You need sea-orm-cli to run this script

sea-orm-cli generate entity \
  --database-url "sqlite://./db.sqlite" \
  --output-dir src/db/entity \
  --with-serde both
