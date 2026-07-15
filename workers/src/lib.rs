//! discord botからD1とVectorizeを叩くためのWorker。
//!
//! ボットからの HTTP リクエストを受け、D1(本文・メタデータ)とVectorize(embeddings)を束ねて返す。
//! embeddingsの生成にはOpenAIのtext-embedding-3-smallを使う。

use worker::*;

#[event(fetch)]
async fn fetch(_req: Request, _env: Env, _ctx: Context) -> Result<Response> {
    Response::ok("udamanami-db-api: ok")
}
