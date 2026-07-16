//! rig を駆動して実際にモデル応答を生成する実行エンジン。
//! `AgentRun` を用いたツール付き複数ターンの会話と、1回だけの補完を提供する。
//! rig の実行時型への依存はこのモジュールに閉じ込める。

use std::collections::BTreeSet;

use anyhow::Result;
use serde_json::json;

use rig::agent::run::{AgentRun, AgentRunStep, ModelTurn, ModelTurnOutcome};
use rig::client::CompletionClient;
use rig::completion::{CompletionModel, Message as RigMessage, ToolDefinition};
use rig::message::{ToolResultContent, UserContent};
use rig::providers::openai;
use rig::OneOrMany;

use super::decorate_output::{compress, Block};
use super::message::ChatMessage;
use super::tools;

/// `AgentRun` を使って、ツール呼び出しを含む複数ターンの会話を進める。
pub async fn run_agent(
    client: &openai::Client,
    model: &str,
    effort: &str,
    system_prompt: &str,
    messages: Vec<ChatMessage>,
    db: &crate::db::BotDatabase,
    target_user_id: &str,
) -> Result<String> {
    let history: Vec<RigMessage> = messages.iter().map(ChatMessage::to_rig).collect();
    let prompt = history
        .last()
        .ok_or_else(|| anyhow::anyhow!("no messages to send"))?
        .clone();

    let tool_defs = tools::definitions();
    let tool_names: BTreeSet<String> = tool_defs
        .iter()
        .map(|t: &ToolDefinition| t.name.clone())
        .collect();

    let mut run = AgentRun::new(prompt).with_history(history).max_turns(10);

    let mut response_blocks: Vec<Block> = Vec::new(); // まなみの reasoning 表示用

    loop {
        match run.next_step()? {
            // LLMによる応答
            AgentRunStep::CallModel {
                prompt, history, ..
            } => {
                let cm = client.completion_model(model);
                let request = cm
                    .completion_request(prompt)
                    .messages(history)
                    .preamble(system_prompt.to_owned())
                    .tools(tool_defs.clone()) // ここでツールを登録
                    .additional_params(json!({ "reasoning": { "effort": effort } }))
                    .build();

                let resp = cm.completion(request).await?;

                response_blocks.extend(compress(resp.choice.clone()));

                // モデルの結果を機械に返す
                let turn = ModelTurn::new(
                    resp.message_id,
                    resp.choice,
                    resp.usage,
                    tool_names.clone(), // executable_tool_names
                    tool_names.clone(), // allowed_tool_names
                );

                match run.model_response(turn)? {
                    ModelTurnOutcome::Continue { .. } => {} // 通常はこれ。次の next_step へ
                    ModelTurnOutcome::TurnRetried => {} // リトライ挿入済み。next_step が再度 CallModel を返す
                    ModelTurnOutcome::NeedsResolution(_) => {} // TODO: どうにかする
                }
            }

            // ツール呼び出し
            AgentRunStep::CallTools { calls } => {
                let mut results = vec![];

                for pending in calls {
                    // retryとかで結果が既に確定しているものは、実行せずそのまま返す
                    if let Some(pre) = pending.preresolved_result {
                        results.push(pre);
                        continue;
                    }

                    // ツール実行ここから
                    let call = pending.tool_call;
                    // 引数は痕跡表示用にコンパクト JSON で控えておく（dispatch で move する前に取る）。
                    let args = call.function.arguments.to_string();
                    let output = match tools::dispatch(
                        db,
                        target_user_id,
                        &call.function.name,
                        call.function.arguments,
                    )
                    .await
                    {
                        Ok(output) => output,
                        Err(e) => {
                            format!(
                                "ツール {} の呼び出しに失敗したよ。Error: {}",
                                call.function.name, e
                            )
                        }
                    };

                    response_blocks.push(Block::ToolCall {
                        name: call.function.name,
                        args,
                    });

                    // 結果は呼び出し ID と 1 対 1 で対応させる。
                    let content = OneOrMany::one(ToolResultContent::text(output));
                    let result = if let Some(call_id) = call.call_id {
                        UserContent::tool_result_with_call_id(call.id, call_id, content)
                    } else {
                        UserContent::tool_result(call.id, content)
                    };
                    results.push(result);
                }

                run.tool_results(results)?; // 積んで次の next_step へ（= 再びモデル呼び出し）
            }

            // 最終ステップ
            AgentRunStep::Done(_) => {
                let out = response_blocks
                    .into_iter()
                    .map(Block::decorate)
                    .collect::<Vec<_>>();
                return Ok(out.join("\n"));
            }
        }
    }
}

/// `CompletionClient` を使って1回だけのメッセージを生成する。ツールは使わない。
pub async fn run_completion(
    client: &openai::Client,
    model: &str,
    effort: &str,
    preamble: &str,
    messages: Vec<ChatMessage>,
) -> Result<String> {
    let mut rig_messages: Vec<RigMessage> = messages.iter().map(ChatMessage::to_rig).collect();

    // rig の builder は prompt を末尾メッセージとして付けるので、
    // バッファ末尾を prompt、それ以前を chat_history に割り当てる。
    let prompt = rig_messages
        .pop()
        .ok_or_else(|| anyhow::anyhow!("no messages to send"))?;

    let completion_model = client.completion_model(model);
    let request = completion_model
        .completion_request(prompt)
        .messages(rig_messages)
        .preamble(preamble.to_owned())
        .additional_params(json!({"reasoning": {"effort": effort}}))
        .build();

    let response = completion_model.completion(request).await?;

    let reply = compress(response.choice)
        .into_iter()
        .map(Block::decorate)
        .collect::<Vec<_>>()
        .join("\n");
    Ok(reply)
}
