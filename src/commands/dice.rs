use crate::commands::CommandContext;
use crate::parser::{cmp_with_operator, parse_dice, Dice};
use nom::{
    error::{Error, ErrorKind},
    Finish as _,
};

use serenity::{http::Http, model::id::ChannelId, utils::MessageBuilder};

pub async fn run(ctx: &CommandContext<'_>) {
    match parse_dice(&ctx.command).finish() {
        Ok((_, parsed)) => {
            dice(ctx.channel_id, ctx.cache_http(), parsed).await;
        }
        Err(Error {
            code: ErrorKind::MapRes,
            ..
        }) => {
            ctx.channel_id
                .say(ctx.cache_http(), "数字がおかしいよ")
                .await
                .unwrap();
        }
        Err(_) => {
            ctx.channel_id
                .say(ctx.cache_http(), "しらないコマンドだよ")
                .await
                .unwrap();
        }
    };
}

async fn dice(reply: &ChannelId, http: &Http, parsed: Dice) {
    // パース
    let Dice { num, dice, cmp } = parsed;

    // 入力のチェック
    if num > 1000000 {
        reply.say(http, "そんないっぱい振れないよ").await.unwrap();
        return;
    } else if num == 0 {
        reply.say(http, "じゃあ振らないよ").await.unwrap();
        return;
    }

    // ダイスロール
    let mut sum = 0;
    let mut vec = vec![];
    for _ in 0..num {
        let r = rand::random::<u64>() % dice + 1;
        vec.push(r.to_string());
        sum += u128::from(r);
    }
    // 結果
    let roll_result = format!("{}D{} -> {}", num, dice, sum);
    // 内訳
    let roll_items = format!(" ({})", vec.join(", "));

    // 比較オプション
    let operation_result = cmp.map(|(operator, operand)| {
        let is_ok = cmp_with_operator(&operator, sum, operand);
        let is_ok = if is_ok { "OK" } else { "NG" };
        format!(" {} {} -> {}", Into::<&str>::into(operator), operand, is_ok)
    });

    // メッセージの生成と送信
    let mut res = MessageBuilder::new();
    res.push(roll_result);
    if 1 < dice && roll_items.len() <= 100 {
        res.push(roll_items);
    }
    if let Some(operation_result) = operation_result {
        res.push(operation_result);
    }
    reply.say(http, &res.build()).await.unwrap();
}
