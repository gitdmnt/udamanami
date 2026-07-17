-- Migration number: 0003 	 2026-07-17T00:00:00.000Z

-- まなみの「自発反応」(話しかけられていない発言へのランダム反応)をチャンネルごとに制御する。
-- reply_enabled は許可フラグ、reply_rate は反応する割合(%)。
-- いずれも NULL は「未設定」で、実行時に既定値(debug は許可・30%、他は不許可)へ解決する。
-- 既存の upsert_channel は列を明示して is_thread/name だけ更新するため、
-- これらのカラムは自動 upsert に上書きされない。

ALTER TABLE "channel" ADD COLUMN "reply_enabled" INTEGER; -- 0/1, NULL=未設定
ALTER TABLE "channel" ADD COLUMN "reply_rate" INTEGER;    -- 0..=100(%), NULL=未設定
