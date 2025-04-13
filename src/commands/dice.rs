use crate::commands::CommandContext;
use crate::parser::{cmp_with_operator, parse_dice, CmpOperator, Dice};
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
            CreateCommandOption::new(CommandOptionType::Integer, "num", "振るサイコロの数")
                .min_int_value(1)
                .max_int_value(1000000),
        )
        .add_option(
            CreateCommandOption::new(CommandOptionType::Integer, "dice", "サイコロの面の数")
                .min_int_value(1)
                .max_int_value(9007199254740991),
        )
        .add_option(
            CreateCommandOption::new(CommandOptionType::String, "operator", "比較方法")
                .required(false)
                .add_string_choice(">", ">")
                .add_string_choice(">=", ">=")
                .add_string_choice("<", "<")
                .add_string_choice("<=", "<=")
                .add_string_choice("=", "=")
                .add_string_choice("!=", "!="),
        )
        .add_option(
            CreateCommandOption::new(CommandOptionType::Integer, "operand", "比較する値")
                .min_int_value(1)
                .max_int_value(9007199254740991),
        )
        .add_option(
            CreateCommandOption::new(CommandOptionType::String, "literal", "ex. 2d6+2 <= 9")
                .required(false),
        )
}

pub fn run(options: &[ResolvedOption]) -> String {
    // parse options

    let dice = if let ResolvedValue::String(s) = options[4].value {
        parse_dice(s).finish().map(|(_, parsed)| parsed)
    } else {
        let num = match options[0].value {
            ResolvedValue::Integer(i) => i as u32,
            _ => 1,
        };
        let dice = match options[1].value {
            ResolvedValue::Integer(i) => i as u64,
            _ => 6,
        };
        let operator = match options[2].value {
            ResolvedValue::String(s) => match s {
                ">" | ">=" | "<" | "<=" | "=" | "==" | "===" | "!=" | "!==" => Some(s.into()),
                _ => None,
            },
            _ => None,
        };
        let operand = match options[3].value {
            ResolvedValue::Integer(i) => Some(i as u128),
            _ => None,
        };

        let cmp: Option<(CmpOperator, u128)> = if operator.is_some() && operand.is_some() {
            #[allow(clippy::unnecessary_unwrap)]
            Some((operator.unwrap(), operand.unwrap()))
        } else {
            None
        };

        Ok(Dice { num, dice, cmp })
    };

    let Dice { num, dice, cmp } = match dice {
        Ok(dice) => dice,
        Err(Error {
            code: ErrorKind::MapRes,
            ..
        }) => return "数字がおかしいよ".to_owned(),
        Err(_) => return "しらないコマンドだよ".to_owned(),
    };

    // roll dice
    let mut res: Vec<u64> = Vec::new();
    let mut sum: u128 = 0;
    for _ in 0..num {
        let r = rand::random::<u64>() % dice + 1;
        res.push(r);
        sum += r as u128;
    }

    // format dice roll result
    let mut result = format!("{}D{} -> {}", num, dice, sum);
    if num > 1 {
        result.push_str(&format!(
            " ({})",
            res.iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        ));
    }

    // compare result if exists
    if let Some(cmp) = cmp {
        let cmp_result = match cmp.0 {
            CmpOperator::GreaterThan => sum > cmp.1,
            CmpOperator::GreaterEqual => sum >= cmp.1,
            CmpOperator::LessThan => sum < cmp.1,
            CmpOperator::LessEqual => sum <= cmp.1,
            CmpOperator::Equal => sum == cmp.1,
            CmpOperator::NotEqual => sum != cmp.1,
        };
        result.push_str(&format!(
            " {} {} -> {}",
            cmp.0,
            cmp.1,
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
        format!(" {} {} -> {}", operator, operand, is_ok)
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
