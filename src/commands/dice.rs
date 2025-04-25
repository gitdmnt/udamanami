use crate::commands::CommandContext;
use crate::parser::{parse_dice, CmpOperator, Dice};
use nom::{
    error::{Error, ErrorKind},
    Finish as _,
};
use serenity::{
    builder::{CreateCommand, CreateCommandOption},
    model::application::{CommandOptionType, ResolvedOption, ResolvedValue},
};

use serenity::{http::Http, model::id::ChannelId, utils::MessageBuilder};

use crate::commands::{StManamiPrefixCommand, StManamiSlashCommand};

pub const PREFIX_DICE_COMMAND: StManamiPrefixCommand = StManamiPrefixCommand {
    name: "",
    usage: "![n]d<m>",
    description: "m面ダイスをn回振るよ！",
    run: |ctx, _| Box::pin(run_old(ctx)),
    is_dm_command: true,
    is_guild_command: true,
};

pub const SLASH_DICE_COMMAND: StManamiSlashCommand = StManamiSlashCommand {
    name: "dice",
    usage: "/dice <operation>",
    description: "サイコロを振るよ！　ex. 2d6 <= 9",
    register,
    run: |options, _| {
        let result = run(options);
        Box::pin(async move { result })
    },
    is_local_command: false,
};

// slash command
pub fn register() -> CreateCommand {
    CreateCommand::new("dice")
        .description("サイコロを振るよ")
        .add_option(
            CreateCommandOption::new(CommandOptionType::String, "literal", "ex. 2d6 <= 9")
                .required(false),
        )
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
}

pub fn run(options: Vec<ResolvedOption>) -> String {
    run_body(parse_options(options))
}

fn parse_options(options: Vec<ResolvedOption>) -> Result<Dice, &str> {
    // parse options
    let (literal, num, dice, operator, operand) = options.iter().fold(
        (None, None, None, None, None),
        |(lit, n, d, op, opd), option| match (option.name, &option.value) {
            ("literal", ResolvedValue::String(s)) => (Some(*s), n, d, op, opd),
            ("num", ResolvedValue::Integer(i)) => (lit, Some(*i as u32), d, op, opd),
            ("dice", ResolvedValue::Integer(i)) => (lit, n, Some(*i as u64), op, opd),
            ("operator", ResolvedValue::String(s)) => (lit, n, d, Some(*s), opd),
            ("operand", ResolvedValue::Integer(i)) => (lit, n, d, op, Some(*i as u128)),
            _ => (lit, n, d, op, opd),
        },
    );

    let dice = literal.map_or_else(
        || {
            let num = num.map_or(1, |i| i);
            let dice = dice.map_or(6, |i| i);
            let operator = operator.map(|s| s.into());
            let cmp: Option<(CmpOperator, u128)> = if operator.is_some() && operand.is_some() {
                #[allow(clippy::unnecessary_unwrap)]
                Some((operator.unwrap(), operand.unwrap()))
            } else {
                None
            };

            Ok(Dice { num, dice, cmp })
        },
        |s| parse_dice(s).finish().map(|(_, parsed)| parsed),
    );

    match dice {
        Ok(dice) => Ok(dice),
        Err(Error {
            code: ErrorKind::MapRes,
            ..
        }) => Err("数字がおかしいよ"),
        Err(_) => Err("しらないコマンドだよ"),
    }
}

fn run_body(resdice: Result<Dice, &str>) -> String {
    let (num, dice, cmp) = match resdice {
        Ok(dice) => (dice.num, dice.dice, dice.cmp),
        Err(err) => return err.to_owned(),
    };

    // roll dice
    let mut res = vec![];
    let mut sum: u128 = 0;
    for _ in 0..num {
        let r = rand::random::<u64>() % dice + 1;
        res.push(r.to_string());
        sum += r as u128;
    }

    // format dice roll result
    let mut result = format!("{}D{} -> {}", num, dice, sum);
    if num > 1 {
        result.push_str(&format!(" ({})", res.join(", ")));
    }

    // compare result if exists
    if let Some(cmp) = cmp {
        let cmp_result = cmp.0.cmp(sum, cmp.1);
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

// old command
pub async fn run_old(ctx: CommandContext<'_>) {
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
        let is_ok = operator.cmp(sum, operand);
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
