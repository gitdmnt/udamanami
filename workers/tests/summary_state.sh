#!/bin/sh
#
# migration 0005 の「SQL としての意味」だけを検査する。
# 素の sqlite3 を使うので、D1 の API 層の上限(1文あたり bind パラメータ 100 個)は検査できない。
# バインド数の不変条件は shared/src/lib.rs の confirm_pending_chunks のテストが守る。
# D1 の実際の上限が 100 のままかは、実 D1 に投げて確かめるしかない。
#
set -eu

repo_dir=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
db_file=$(mktemp "${TMPDIR:-/tmp}/udamanami-summary-state.XXXXXX")
trap 'rm -f "$db_file"' EXIT HUP INT TERM
cd "$repo_dir"

expect() {
    actual=$(sqlite3 "$db_file" "$1")
    if [ "$actual" != "$2" ]; then
        echo "expected '$2', got '$actual'" >&2
        exit 1
    fi
}

sqlite3 "$db_file" <<'SQL'
.read workers/migrations/0001_create_tables.sql
.read workers/migrations/0002_add_user_profile.sql
.read workers/migrations/0003_add_channel_reply.sql

INSERT INTO user (user_id, username) VALUES ('u', 'user');
INSERT INTO channel (channel_id, is_thread, name) VALUES ('backfill', 0, 'backfill');
INSERT INTO message (message_id, channel_id, user_id, content, timestamp) VALUES
    ('old', 'backfill', 'u', 'old', '2026-01-01T00:00:00Z'),
    ('p1', 'backfill', 'u', 'pending', '2026-01-02T00:00:00Z'),
    ('p2', 'backfill', 'u', 'pending', '2026-01-03T00:00:00Z');

.read workers/migrations/0004_add_channel_summary.sql
UPDATE channel SET last_summarized_at = '2026-01-01T00:00:00Z'
WHERE channel_id = 'backfill';
.read workers/migrations/0005_add_channel_summary_state.sql
SQL

# Backfill uses the legacy timestamp-only cursor once, then state is defined by
# the exact per-message pending flag.
expect \
    "SELECT pending_count || '|' || first_pending_message_id || '|' || last_pending_message_id FROM channel WHERE channel_id = 'backfill'" \
    "2|p1|p2"
expect \
    "SELECT group_concat(message_id, ',') FROM (SELECT message_id FROM message WHERE summary_pending = 1 ORDER BY message_id)" \
    "p1,p2"

# Every newly observed row is pending regardless of timestamp; a duplicate
# message_id upsert still does not double-count.
sqlite3 "$db_file" <<'SQL'
CREATE TABLE channel_update_audit (channel_id TEXT NOT NULL);
CREATE TRIGGER audit_channel_update AFTER UPDATE ON channel
BEGIN
    INSERT INTO channel_update_audit VALUES (NEW.channel_id);
END;
INSERT INTO message (message_id, channel_id, user_id, content, timestamp)
VALUES ('history', 'backfill', 'u', 'history', '2025-01-01T00:00:00Z');
DELETE FROM message WHERE message_id = 'history';
SQL
expect "SELECT COUNT(*) FROM channel_update_audit" "2"
sqlite3 "$db_file" <<'SQL'
INSERT INTO message (message_id, channel_id, user_id, content, timestamp)
VALUES ('p3', 'backfill', 'u', 'pending', '2026-01-04T00:00:00Z');
INSERT INTO message (message_id, channel_id, user_id, content, timestamp)
VALUES ('p3', 'backfill', 'u', 'edited', '2026-01-04T00:00:00Z')
ON CONFLICT(message_id) DO UPDATE SET content = excluded.content;
SQL
expect "SELECT pending_count FROM channel WHERE channel_id = 'backfill'" "3"
expect "SELECT COUNT(*) FROM channel_update_audit" "3"

# Blocker 1: during Worker-first rollout, a timestamp-only old-app update must
# not consume any of 201 tied rows. The new app later confirms exactly 200 IDs.
sqlite3 "$db_file" <<'SQL'
INSERT INTO channel (channel_id, is_thread, name) VALUES ('rollout', 0, 'rollout');
WITH RECURSIVE numbers(n) AS (
    SELECT 1 UNION ALL SELECT n + 1 FROM numbers WHERE n < 201
)
INSERT INTO message (message_id, channel_id, user_id, content, timestamp)
SELECT printf('r-%04d', n), 'rollout', 'u', 'same timestamp', '2020-01-01T00:00:00Z'
FROM numbers;

-- This is the dangerous legacy update. State is recomputed from flags, not
-- inferred from its timestamp, so all 201 rows remain pending.
UPDATE channel SET last_summarized_at = '2020-01-01T00:00:00Z'
WHERE channel_id = 'rollout';
SQL
expect \
    "SELECT pending_count || '|' || first_pending_message_id || '|' || last_pending_message_id FROM channel WHERE channel_id = 'rollout'" \
    "201|r-0001|r-0201"

sqlite3 "$db_file" <<'SQL'
UPDATE message SET summary_pending = 0
WHERE message_id IN (
    SELECT message_id FROM message
    WHERE channel_id = 'rollout' AND summary_pending = 1
    ORDER BY timestamp, message_id LIMIT 200
);
UPDATE channel SET
    last_summarized_at = '2020-01-01T00:00:00Z',
    last_summarized_message_id = 'r-0200'
WHERE channel_id = 'rollout';
SQL
expect \
    "SELECT pending_count || '|' || first_pending_message_id || '|' || last_pending_message_id FROM channel WHERE channel_id = 'rollout'" \
    "1|r-0201|r-0201"

# Blocker 2: establish a cursor first, then insert a row older than that cursor
# after the next fetch. Exact confirmation must leave the delayed row pending.
sqlite3 "$db_file" <<'SQL'
INSERT INTO channel (channel_id, is_thread, name) VALUES ('delay', 0, 'delay');
INSERT INTO message (message_id, channel_id, user_id, content, timestamp) VALUES
    ('d-100', 'delay', 'u', 'bootstrap', '2022-01-01T00:01:00Z'),
    ('d-300', 'delay', 'u', 'fetched',   '2022-01-01T00:03:00Z'),
    ('d-400', 'delay', 'u', 'next',    '2022-01-01T00:03:00Z');

-- First establish a non-NULL cursor at d-100.
UPDATE message SET summary_pending = 0
WHERE channel_id = 'delay' AND message_id = 'd-100';
UPDATE channel SET
    last_summarized_at = '2022-01-01T00:01:00Z',
    last_summarized_message_id = 'd-100'
WHERE channel_id = 'delay';

-- GET has now returned d-300. Delayed delivery is older than the already
-- established d-100 cursor, but must still become pending.
INSERT INTO message (message_id, channel_id, user_id, content, timestamp)
VALUES ('d-050', 'delay', 'u', 'delayed', '2022-01-01T00:00:00Z');

-- The Worker confirms only the ID supplied by that GET.
UPDATE message SET summary_pending = 0
WHERE channel_id = 'delay' AND summary_pending = 1
  AND message_id IN ('d-300');
UPDATE channel SET
    last_summarized_at = '2022-01-01T00:03:00Z',
    last_summarized_message_id = 'd-300'
WHERE channel_id = 'delay';
SQL
expect \
    "SELECT pending_count || '|' || first_pending_message_id || '|' || last_pending_message_id FROM channel WHERE channel_id = 'delay'" \
    "2|d-050|d-400"
expect \
    "SELECT group_concat(message_id, ',') FROM (SELECT message_id FROM message WHERE channel_id = 'delay' AND summary_pending = 1 ORDER BY timestamp, message_id LIMIT 200)" \
    "d-050,d-400"

# Deleting a pending row updates state; deleting processed history is a no-op.
sqlite3 "$db_file" <<'SQL'
DELETE FROM channel_update_audit;
DELETE FROM message WHERE message_id = 'd-050';
DELETE FROM message WHERE message_id = 'd-100';
SQL
expect "SELECT pending_count || '|' || first_pending_message_id FROM channel WHERE channel_id = 'delay'" "1|d-400"
expect "SELECT COUNT(*) FROM channel_update_audit" "1"

# Candidate polling remains channel-only and preserves first-pending order.
expect \
    "SELECT channel_id FROM channel WHERE pending_count > 0 AND (last_pending_at <= '2099-01-01T00:00:00Z' OR pending_count >= 100) ORDER BY first_pending_at, first_pending_message_id LIMIT 1" \
    "rollout"

# All denormalized fields agree with the exact pending set.
expect \
    "SELECT COUNT(*) FROM channel AS c WHERE pending_count != (SELECT COUNT(*) FROM message AS m WHERE m.channel_id = c.channel_id AND m.summary_pending = 1) OR COALESCE(first_pending_at || '|' || first_pending_message_id, '') != COALESCE((SELECT m.timestamp || '|' || m.message_id FROM message AS m WHERE m.channel_id = c.channel_id AND m.summary_pending = 1 ORDER BY m.timestamp, m.message_id LIMIT 1), '') OR COALESCE(last_pending_at || '|' || last_pending_message_id, '') != COALESCE((SELECT m.timestamp || '|' || m.message_id FROM message AS m WHERE m.channel_id = c.channel_id AND m.summary_pending = 1 ORDER BY m.timestamp DESC, m.message_id DESC LIMIT 1), '')" \
    "0"

echo "summary state tests passed"
