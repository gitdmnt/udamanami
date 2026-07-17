//! workers (D1) の HTTP API を叩く薄いクライアント。
//!
//! ID などの型は shared の DTO(文字列)をそのまま使い、
//! serenity 型との変換は呼び出し側(dbサービス層)に任せる。

use serde::de::DeserializeOwned;
use serde::Serialize;
use udamanami_shared::{
    CalcVar, CalcVarWithUsername, Channel, ChannelId, ChannelReplySetting, DeleteCalcVar,
    DeleteMessage, GetMessages, Memory, MemoryDetail, MemoryListItem, MemorySearchResult, Message,
    MessageId, MessageOrder, SetUserProfile, UpdateMemory, UpdateMessage, User, UserId,
    UserProfile,
};

pub struct WorkersApi {
    client: reqwest::Client,
    base_url: String,
    auth_token: String,
}

impl WorkersApi {
    pub fn new(base_url: impl Into<String>, auth_token: impl Into<String>) -> Self {
        Self {
            client: reqwest::Client::new(),
            base_url: base_url.into(),
            auth_token: auth_token.into(),
        }
    }

    // ---------------- メッセージログ ----------------

    /// メッセージを一括でupsertする(POST /message)
    pub async fn insert_messages(&self, messages: &[Message]) -> anyhow::Result<()> {
        self.request_text(reqwest::Method::POST, "/message", Some(messages))
            .await?;
        Ok(())
    }

    /// メッセージ本文を更新する(PUT /message)
    pub async fn update_message(
        &self,
        message_id: MessageId,
        content: String,
    ) -> anyhow::Result<()> {
        let body = UpdateMessage {
            message_id,
            content,
        };
        self.request_text(reqwest::Method::PUT, "/message", Some(&body))
            .await?;
        Ok(())
    }

    /// メッセージを削除する(DELETE /message)
    pub async fn delete_message(&self, message_id: MessageId) -> anyhow::Result<()> {
        let body = DeleteMessage { message_id };
        self.request_text(reqwest::Method::DELETE, "/message", Some(&body))
            .await?;
        Ok(())
    }

    /// 条件(件数上限・並び順・期間)を指定してチャンネルのメッセージを取得する(GET /message)
    pub async fn get_messages(&self, query: GetMessages) -> anyhow::Result<Vec<Message>> {
        let mut params = vec![
            ("channel_id", query.channel_id),
            ("limit", query.limit.to_string()),
        ];
        if let Some(order) = query.order {
            let order = match order {
                MessageOrder::Asc => "Asc",
                MessageOrder::Desc => "Desc",
            };
            params.push(("order", order.to_owned()));
        }
        if let Some(from) = query.from {
            params.push(("from", from));
        }
        if let Some(to) = query.to {
            params.push(("to", to));
        }
        self.get_with_query("/message", &params).await
    }

    // ---------------- ユーザー ----------------

    /// ユーザー名をupsertする(POST /user)
    pub async fn upsert_user(&self, user_id: UserId, username: String) -> anyhow::Result<()> {
        let body = User {
            user_id,
            username,
            room_pointer: None,
        };
        self.request_text(reqwest::Method::POST, "/user", Some(&body))
            .await?;
        Ok(())
    }

    /// 代筆先チャンネルを設定する(PUT /user/room-pointer)
    pub async fn set_room_pointer(
        &self,
        user_id: UserId,
        username: String,
        room_pointer: Option<ChannelId>,
    ) -> anyhow::Result<()> {
        let body = User {
            user_id,
            username,
            room_pointer,
        };
        self.request_text(reqwest::Method::PUT, "/user/room-pointer", Some(&body))
            .await?;
        Ok(())
    }

    /// 代筆先チャンネルを取得する(GET /user/room-pointer)
    pub async fn get_room_pointer(&self, user_id: UserId) -> anyhow::Result<Option<ChannelId>> {
        self.get_with_query("/user/room-pointer", &[("user_id", user_id)])
            .await
    }

    /// 人間プロフィールを部分更新する(PUT /user/profile)
    pub async fn set_user_profile(&self, body: &SetUserProfile) -> anyhow::Result<()> {
        self.request_text(reqwest::Method::PUT, "/user/profile", Some(body))
            .await?;
        Ok(())
    }

    /// 人間プロフィールを取得する(GET /user/profile)。未登録なら None。
    pub async fn get_user_profile(&self, user_id: UserId) -> anyhow::Result<Option<UserProfile>> {
        self.get_with_query("/user/profile", &[("user_id", user_id)])
            .await
    }

    // ---------------- チャンネル ----------------

    /// チャンネル情報をupsertする(POST /channel)
    pub async fn upsert_channel(&self, channel: &Channel) -> anyhow::Result<()> {
        self.request_text(reqwest::Method::POST, "/channel", Some(channel))
            .await?;
        Ok(())
    }

    /// 自発反応の設定を部分更新する(PUT /channel/reply)。None のフィールドは据え置き。
    pub async fn set_channel_reply_setting(
        &self,
        setting: &ChannelReplySetting,
    ) -> anyhow::Result<()> {
        self.request_text(reqwest::Method::PUT, "/channel/reply", Some(setting))
            .await?;
        Ok(())
    }

    /// 自発反応の設定を取得する(GET /channel/reply)。未登録なら None。
    pub async fn get_channel_reply_setting(
        &self,
        channel_id: ChannelId,
    ) -> anyhow::Result<Option<ChannelReplySetting>> {
        self.get_with_query("/channel/reply", &[("channel_id", channel_id)])
            .await
    }

    // ---------------- calc var ----------------

    /// 変数をupsertする(POST /calcvar)
    pub async fn upsert_calc_var(&self, var: &CalcVar) -> anyhow::Result<()> {
        self.request_text(reqwest::Method::POST, "/calcvar", Some(var))
            .await?;
        Ok(())
    }

    /// 全変数を取得する(GET /calcvar、EvalContextの復元用)
    pub async fn get_all_calc_vars(&self) -> anyhow::Result<Vec<CalcVar>> {
        self.get_with_query("/calcvar", &[]).await
    }

    /// 変数を削除する(DELETE /calcvar)
    pub async fn delete_calc_var(&self, var_name: String) -> anyhow::Result<()> {
        let body = DeleteCalcVar { var_name };
        self.request_text(reqwest::Method::DELETE, "/calcvar", Some(&body))
            .await?;
        Ok(())
    }

    /// 変数名と所有者名の一覧を取得する(GET /calcvar/list)
    pub async fn list_calc_vars(&self) -> anyhow::Result<Vec<CalcVarWithUsername>> {
        self.get_with_query("/calcvar/list", &[]).await
    }

    // ---------------- メモリ ----------------

    pub async fn create_memory(&self, memory: &Memory) -> anyhow::Result<()> {
        self.request_text(reqwest::Method::POST, "/memory", Some(memory))
            .await?;
        Ok(())
    }

    pub async fn update_memory(&self, update: &UpdateMemory) -> anyhow::Result<()> {
        self.request_text(reqwest::Method::PUT, "/memory", Some(update))
            .await?;
        Ok(())
    }

    pub async fn delete_memory(&self, memory_id: String) -> anyhow::Result<()> {
        self.request_text(reqwest::Method::DELETE, "/memory", Some(&memory_id))
            .await?;
        Ok(())
    }

    pub async fn search_memory(
        &self,
        query: &str,
        limit: usize,
    ) -> anyhow::Result<Vec<MemorySearchResult>> {
        let params = [("q", query.to_owned()), ("limit", limit.to_string())];
        self.get_with_query("/memory/search", &params).await
    }

    /// メモリ一覧を取得する(GET /memory/list)
    pub async fn list_memories(&self) -> anyhow::Result<Vec<MemoryListItem>> {
        self.get_with_query("/memory/list", &[]).await
    }

    /// メモリ1件の全文を取得する(GET /memory)
    pub async fn get_memory(&self, memory_id: String) -> anyhow::Result<MemoryDetail> {
        let params = [("memory_id", memory_id)];
        self.get_with_query("/memory", &params).await
    }

    // ---------------- 内部ヘルパ ----------------

    /// GETはボディではなくクエリパラメータで条件を渡す
    async fn get_with_query<T: DeserializeOwned>(
        &self,
        path: &str,
        params: &[(&str, String)],
    ) -> anyhow::Result<T> {
        let response = self
            .client
            .get(format!("{}{}", self.base_url, path))
            .query(params)
            .bearer_auth(&self.auth_token)
            .send()
            .await?;
        if !response.status().is_success() {
            let status = response.status();
            let text = response.text().await.unwrap_or_default();
            anyhow::bail!("workers API error: {path} returned {status}: {text}");
        }
        Ok(response.json().await?)
    }

    async fn send<B: Serialize + ?Sized + Sync>(
        &self,
        method: reqwest::Method,
        path: &str,
        body: Option<&B>,
    ) -> anyhow::Result<reqwest::Response> {
        let mut request = self
            .client
            .request(method, format!("{}{}", self.base_url, path))
            .bearer_auth(&self.auth_token);
        if let Some(body) = body {
            request = request.json(body);
        }

        let response = request.send().await?;
        if !response.status().is_success() {
            let status = response.status();
            let text = response.text().await.unwrap_or_default();
            anyhow::bail!("workers API error: {path} returned {status}: {text}");
        }
        Ok(response)
    }

    async fn request_text<B: Serialize + ?Sized + Sync>(
        &self,
        method: reqwest::Method,
        path: &str,
        body: Option<&B>,
    ) -> anyhow::Result<String> {
        Ok(self.send(method, path, body).await?.text().await?)
    }
}
