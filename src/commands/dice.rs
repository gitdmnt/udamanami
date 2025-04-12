use crate::commands::CommandContext;
use crate::parser::{cmp_with_operator, parse_dice, Dice};
use nom::{
    error::{Error, ErrorKind},
    Finish as _,
};
use serenity::{
    builder::{CreateCommand, CreateCommandOption},
    model::application::{CommandOptionType, ResolvedOption, ResolvedValue},
};

use serenity::{http::Http, model::id::ChannelId, utils::MessageBuilder};

pub fn register() -> CreateCommand {
    CreateCommand::new("dice")
        .description("サイコロを振るよ")
        .add_option(
            CreateCommandOption::new(CommandOptionType::Integer, "count", "振るサイコロの数")
                .min_int_value(1)
                .max_int_value(1000000),
        )
        .add_option(
            CreateCommandOption::new(CommandOptionType::Integer, "sides", "サイコロの面の数")
                .min_int_value(1)
                .max_int_value(9007199254740991),
        )
        .add_option(
            CreateCommandOption::new(CommandOptionType::String, "compare_method", "比較方法")
                .required(false)
                .add_string_choice(">", ">")
                .add_string_choice(">=", ">=")
                .add_string_choice("<", "<")
                .add_string_choice("<=", "<=")
                .add_string_choice("=", "=")
                .add_string_choice("!=", "!="),
        )
        .add_option(
            CreateCommandOption::new(CommandOptionType::Integer, "compare_value", "比較する値")
                .min_int_value(1)
                .max_int_value(9007199254740991),
        )
}

pub fn run(options: &[ResolvedOption]) -> String {
    // parse options
    let count: u64 = if let ResolvedValue::Integer(i) = options[0].value {
        i as u64
    } else {
        1
    };
    let sides = if let ResolvedValue::Integer(i) = options[1].value {
        i as u64
    } else {
        6
    };
    let compare_method = if let ResolvedValue::String(s) = options[2].value {
        Some(s)
    } else {
        None
    };
    let compare_value = if let ResolvedValue::Integer(i) = options[3].value {
        Some(i as u128)
    } else {
        None
    };

    // roll dice
    let mut res: Vec<u64> = Vec::new();
    let mut sum: u128 = 0;
    for _ in 0..count {
        let r = rand::random::<u64>() % sides + 1;
        res.push(r);
        sum += r as u128;
    }

    // format dice roll result
    let mut result = format!("{}D{} -> {}", count, sides, sum);
    if count > 1 {
        result.push_str(&format!(
            " ({})",
            res.iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        ));
    }

    // compare result if exists
    if let Some(method) = compare_method {
        let Some(cmp_value) = compare_value else {
            return result;
        };
        let cmp_result = match method {
            ">" => sum > cmp_value,
            ">=" => sum >= cmp_value,
            "<" => sum < cmp_value,
            "<=" => sum <= cmp_value,
            "=" => sum == cmp_value,
            "!=" => sum != cmp_value,
            _ => false,
        };
        result.push_str(&format!(
            " {} {} -> {}",
            method,
            cmp_value,
            if cmp_result { "OK" } else { "NG" }
        ));
    }

    // return result
    result
}

pub async fn run_old(ctx: &CommandContext<'_>) {
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
