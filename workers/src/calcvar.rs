//! 計算機の変数のCRUD

use serde::{Deserialize, Serialize};
use worker::*;

use crate::user::UserId;

#[derive(Debug, Deserialize, Serialize)]
struct CalcVar {
    var_name: String,
    var_value: String,
    user_id: UserId,
}

#[derive(Debug, Deserialize, Serialize)]
struct DeleteCalcVar {
    var_name: String,
}

#[derive(Debug, Deserialize, Serialize)]
struct CalcVarWithUsername {
    var_name: String,
    username: Option<String>,
}

pub async fn upsert_var(mut req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let body = req.text().await?;
    let Ok(var) = serde_json::from_str::<CalcVar>(&body) else {
        return Response::error("Failed to parse request body", 400);
    };

    let d1 = ctx.env.d1("DB")?;

    let _ = d1
        .prepare(
            "INSERT INTO calc_var (var_name, var_value, user_id) VALUES (?, ?, ?)
ON CONFLICT (var_name) DO UPDATE SET var_value = excluded.var_value;
",
        )
        .bind(&[var.var_name.into(), var.var_value.into(), var.user_id.into()])?
        .run()
        .await?;

    Response::ok("Var upserted successfully")
}

/// 全変数を取得する(EvalContextの復元用)
pub async fn get_vars(_req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let d1 = ctx.env.d1("DB")?;

    let result = d1.prepare("SELECT * FROM calc_var").run().await?;

    Response::from_json(&result.results::<CalcVar>()?)
}

pub async fn delete_var(mut req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let body = req.text().await?;
    let Ok(var) = serde_json::from_str::<DeleteCalcVar>(&body) else {
        return Response::error("Failed to parse request body", 400);
    };

    let d1 = ctx.env.d1("DB")?;

    let _ = d1
        .prepare("DELETE FROM calc_var WHERE var_name = ?")
        .bind(&[var.var_name.into()])?
        .run()
        .await?;

    Response::ok("Var deleted successfully")
}

/// 変数名と所有者のユーザー名の一覧を取得する
pub async fn list_vars(_req: Request, ctx: RouteContext<()>) -> Result<Response> {
    let d1 = ctx.env.d1("DB")?;

    let result = d1
        .prepare(
            "SELECT calc_var.var_name, user.username FROM calc_var
LEFT JOIN user ON calc_var.user_id = user.user_id
ORDER BY user.username ASC",
        )
        .run()
        .await?;

    Response::from_json(&result.results::<CalcVarWithUsername>()?)
}
