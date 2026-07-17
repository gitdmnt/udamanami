-- Migration number: 0004 	 2026-07-17T00:00:00.000Z

-- チャンネルの会話セッションを自動要約して memory 化するための状態列。
-- last_summarized_at より新しい message が「未要約」で、要約と memory 登録の両方が成功したときだけ前進させる。
-- NULL は「未設定」で、実行時に既定値(直近 MAX_LOOKBACK)へ解決する。
-- 既存の upsert_channel は列を明示して is_thread/name だけ更新するため、このカラムは自動 upsert に上書きされない。

ALTER TABLE "channel" ADD COLUMN "last_summarized_at" TEXT; -- RFC3339, NULL=未設定

-- 既存チャンネルの過去ログ全部を一気に要約対象にしないよう、「現時点までは要約済み」とみなして初期化する。
-- メッセージがまだ無いチャンネルは NULL のまま(= 最初の会話から要約対象)。
-- D1 は適用済みのマイグレーションを再実行しないため、非羃等でもOK
UPDATE "channel"
SET "last_summarized_at" = (
    SELECT MAX("timestamp") FROM "message"
    WHERE "message"."channel_id" = "channel"."channel_id"
)
WHERE EXISTS (
    SELECT 1 FROM "message" WHERE "message"."channel_id" = "channel"."channel_id"
);

-- 候補抽出はチャンネルごとの MIN/MAX(timestamp) と COUNT の走査になるため索引を張る。
CREATE INDEX IF NOT EXISTS "idx_message_channel_id_timestamp"
    ON "message" ("channel_id", "timestamp");

-- 自動要約由来の memory を手動(remember ツール由来)と区別する。NULL=手動/従来。
ALTER TABLE "memory" ADD COLUMN "source" TEXT; -- NULL | 'auto_summary'

-- 会話の出所(どのチャンネルの、いつの会話か)は本文とは別の列で持つ。
-- どのチャンネルの話かで内容が現実かどうかが変わる(夢日記など)ため、recall で返す。
ALTER TABLE "memory" ADD COLUMN "channel_name" TEXT; -- NULL=手動
ALTER TABLE "memory" ADD COLUMN "occurred_at" TEXT;  -- RFC3339, NULL=手動
