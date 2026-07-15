-- Migration number: 0001 	 2026-07-15T10:20:48.959Z

-- general

CREATE TABLE IF NOT EXISTS "channel" (
    "channel_id" TEXT PRIMARY KEY NOT NULL,
    "is_thread" BOOLEAN NOT NULL,
    "name" TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS "user" (
    "user_id" TEXT PRIMARY KEY NOT NULL,
    "username" TEXT NOT NULL,
    "room_pointer" TEXT REFERENCES "channel" ("channel_id")
);

CREATE TABLE IF NOT EXISTS "message" (
    "message_id" TEXT PRIMARY KEY NOT NULL,
    "channel_id" TEXT NOT NULL REFERENCES "channel" ("channel_id"),
    "user_id" TEXT NOT NULL REFERENCES "user" ("user_id"),
    "content" TEXT NOT NULL,
    "timestamp" TEXT NOT NULL
);

-- calc

CREATE TABLE IF NOT EXISTS "calc_var" (
    "var_name" TEXT PRIMARY KEY NOT NULL,
    "var_value" TEXT NOT NULL,
    "user_id" TEXT NOT NULL REFERENCES "user" ("user_id")
);

-- memory
-- memory_id, chunk_idはUUIDv4を想定。chunk_indexは0から始まる連番。

CREATE TABLE IF NOT EXISTS "memory" (
    "memory_id" TEXT PRIMARY KEY NOT NULL, 
    "title" TEXT NOT NULL,
    "timestamp" TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS "memory_chunk" (
    "chunk_id" TEXT PRIMARY KEY NOT NULL,
    "memory_id" TEXT NOT NULL REFERENCES "memory" ("memory_id") ON DELETE CASCADE,
    "chunk_index" INTEGER NOT NULL,
    "content" TEXT NOT NULL
);

CREATE UNIQUE INDEX IF NOT EXISTS "idx_memory_chunk_memory_id_chunk_index"
    ON "memory_chunk" ("memory_id", "chunk_index");
