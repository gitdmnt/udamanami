//! discord botからD1とVectorizeを叩くためのWorker。
//!
//! ボットからの HTTP リクエストを受け、D1(本文・メタデータ)とVectorize(embeddings)を束ねて返す。
//! embeddingsの生成にはOpenAIのtext-embedding-3-smallを使う。

use worker::*;

mod calcvar;
mod channel;
mod memory;
mod message;
mod user;

use constant_time_eq::constant_time_eq;

/// D1バインド用: None は JsValue::UNDEFINED になり D1_TYPE_ERROR を起こすため、
/// 明示的に JsValue::NULL へ落とす。
pub(crate) fn opt_to_js<T: Into<wasm_bindgen::JsValue>>(v: Option<T>) -> wasm_bindgen::JsValue {
    v.map_or(wasm_bindgen::JsValue::NULL, Into::into)
}

pub(crate) fn query_param(url: &Url, key: &str) -> Option<String> {
    url.query_pairs()
        .find(|(k, _)| k == key)
        .map(|(_, v)| v.into_owned())
}

#[event(fetch)]
async fn fetch(req: Request, env: Env, _ctx: Context) -> Result<Response> {
    // 認証
    let expected = env.secret("AUTH_TOKEN")?.to_string();
    let authorized = req
        .headers()
        .get("Authorization")?
        .unwrap_or_default()
        .replace("Bearer ", "");

    if constant_time_eq(authorized.as_bytes(), expected.as_bytes()) {
        return Response::error("Unauthorized", 401);
    }

    // ルーティング
    let router = Router::new();
    router
        .get("/health", |_, _| Response::ok("OK"))
        // メッセージログ
        .post_async("/message", message::insert_messages)
        .put_async("/message", message::update_message)
        .delete_async("/message", message::delete_message)
        .get_async("/message", message::get_messages)
        // ユーザー
        .post_async("/user", user::upsert_user)
        .put_async("/user/room-pointer", user::set_room_pointer)
        .get_async("/user/room-pointer", user::get_room_pointer)
        .put_async("/user/profile", user::set_profile)
        .get_async("/user/profile", user::get_profile)
        // チャンネル
        .post_async("/channel", channel::upsert_channel)
        .put_async("/channel/reply", channel::set_reply_setting)
        .get_async("/channel/reply", channel::get_reply_setting)
        .get_async("/channel/summary/candidates", channel::summarize_candidates)
        .put_async("/channel/summary", channel::set_summarized)
        // 計算機の変数
        .post_async("/calcvar", calcvar::upsert_var)
        .get_async("/calcvar", calcvar::get_vars)
        .delete_async("/calcvar", calcvar::delete_var)
        .get_async("/calcvar/list", calcvar::list_vars)
        // メモリ
        .post_async("/memory", memory::create_memory)
        .put_async("/memory", memory::update_memory)
        .delete_async("/memory", memory::delete_memory)
        .get_async("/memory/search", memory::search_memory)
        .get_async("/memory/list", memory::list_memories)
        .get_async("/memory", memory::get_memory)
        .run(req, env)
        .await
}
