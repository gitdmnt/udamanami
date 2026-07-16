-- Migration number: 0002 	 2026-07-17T00:00:00.000Z

-- まなみが tool 学習する人間プロフィール項目を user テーブルに追加する。
-- いずれも単一の自由記述テキストで、更新は全文置換。未学習は NULL。
-- 既存の upsert_user は列を明示して username/room_pointer だけ更新するため、
-- これらのカラムは自動 upsert に上書きされない。

ALTER TABLE "user" ADD COLUMN "calling_name" TEXT;
ALTER TABLE "user" ADD COLUMN "liked_topics" TEXT;
ALTER TABLE "user" ADD COLUMN "disliked_topics" TEXT;
