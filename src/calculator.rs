use std::collections::{HashMap, HashSet};

use dashmap::DashMap;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{
        alpha1, alphanumeric1, anychar, char, digit1, multispace0, none_of, one_of,
    },
    combinator::{map, recognize, value},
    multi::{fold_many0, many0, many0_count, many1_count, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, tuple},
    IResult,
};
use rand::{prelude::Distribution, Rng};
use rand_distr::StandardNormal;
use strum::{EnumIter, IntoEnumIterator};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprOp2 {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Dice,
    Gt,
    Ge,
    Lt,
    Le,
    Eq,
    Ne,
    AndL,
    OrL,
    XorL,
}

impl std::fmt::Display for ExprOp2 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Mod => write!(f, "%"),
            Self::Pow => write!(f, "^"),
            Self::Dice => write!(f, "d"),
            Self::Gt => write!(f, ">"),
            Self::Ge => write!(f, ">="),
            Self::Lt => write!(f, "<"),
            Self::Le => write!(f, "<="),
            Self::Eq => write!(f, "=="),
            Self::Ne => write!(f, "!="),
            Self::AndL => write!(f, "&&"),
            Self::OrL => write!(f, "||"),
            Self::XorL => write!(f, "^^"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum ExprOp1 {
    Neg,
    OneDice,
    NotL,
}

impl std::fmt::Display for ExprOp1 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Neg => write!(f, "-"),
            Self::OneDice => write!(f, "d"),
            Self::NotL => write!(f, "!"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum Expr {
    IVal(i64),
    FVal(f64),
    BVal(bool),
    SVal(String),
    List(Vec<Expr>),
    At(Box<Expr>, Box<Expr>),
    Object(HashMap<String, Box<Expr>>),
    Get(Box<Expr>, String),
    Const(String),
    Op1(ExprOp1, Box<Expr>),
    Op2(ExprOp2, Box<Expr>, Box<Expr>),
    Apply(Box<Expr>, Vec<Expr>),
    Lambda(Vec<String>, Box<Expr>),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::IVal(i) => write!(f, "{}", i),
            Self::FVal(v) => write!(f, "{}", v),
            Self::BVal(b) => write!(f, "{}", b),
            Self::SVal(s) => write!(f, "\"{}\"", s.escape_debug()),
            Self::List(l) => write!(
                f,
                "[{}]",
                l.iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::At(e1, e2) => write!(f, "{}[{}]", e1, e2),
            Self::Object(o) => write!(
                f,
                "{{{}}}",
                o.iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Get(e, k) => write!(f, "{}.{}", e, k),
            Self::Const(s) => write!(f, "{}", s),
            Self::Op1(op, e) => write!(f, "{}{}", op, e),
            Self::Op2(op, e1, e2) => write!(f, "({} {} {})", e1, op, e2),
            Self::Apply(name, args) => {
                write!(
                    f,
                    "{}({})",
                    name,
                    args.iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Self::Lambda(params, body) => {
                write!(f, "({} => {})", params.join(", "), body)
            }
        }
    }
}

/*
演算子の結合性と優先順位
1. 単項 - D
2. 左結合 D
3. 右結合 ^
4. 左結合 * / %
5. 左結合 + -
*/

fn parse_binop_left(
    op_parser: fn(&str) -> IResult<&str, ExprOp2>,
    next_parser: fn(&str) -> IResult<&str, Expr>,
) -> impl Fn(&str) -> IResult<&str, Expr> {
    move |input: &str| {
        let (input, init) = next_parser(input)?;
        fold_many0(
            pair(preceded(multispace0, op_parser), next_parser),
            move || init.clone(),
            |acc, (op, val)| Expr::Op2(op, Box::new(acc), Box::new(val)),
        )(input)
    }
}

fn parse_binop_right(
    op_parser: fn(&str) -> IResult<&str, ExprOp2>,
    next_parser: fn(&str) -> IResult<&str, Expr>,
) -> impl Fn(&str) -> IResult<&str, Expr> {
    move |input: &str| {
        let (input, init) = next_parser(input)?;
        let (input, (mut exprs, ops)) = fold_many0(
            pair(preceded(multispace0, op_parser), next_parser),
            || (vec![init.clone()], Vec::new()),
            |(mut exprs, mut ops), (op, val)| {
                exprs.push(val);
                ops.push(op);
                (exprs, ops)
            },
        )(input)?;

        let last = exprs.pop().unwrap();

        let result = ops
            .into_iter()
            .rev()
            .zip(exprs.into_iter().rev())
            .fold(last, |acc, (op, val)| {
                Expr::Op2(op, Box::new(val), Box::new(acc))
            });

        Ok((input, result))
    }
}

//無結合 同順位の演算子の連続は受理しない
fn parse_binop_none(
    op_parser: fn(&str) -> IResult<&str, ExprOp2>,
    next_parser: fn(&str) -> IResult<&str, Expr>,
) -> impl Fn(&str) -> IResult<&str, Expr> {
    move |input: &str| {
        map(
            tuple((next_parser, preceded(multispace0, op_parser), next_parser)),
            |(e1, op, e2)| Expr::Op2(op, Box::new(e1), Box::new(e2)),
        )(input)
    }
}

fn parse_term_l(
    op_parser: fn(&str) -> IResult<&str, ExprOp2>,
    next_parser: fn(&str) -> IResult<&str, Expr>,
) -> impl Fn(&str) -> IResult<&str, Expr> {
    move |input: &str| {
        preceded(
            multispace0,
            alt((parse_binop_left(op_parser, next_parser), next_parser)),
        )(input)
    }
}

fn parse_term_r(
    op_parser: fn(&str) -> IResult<&str, ExprOp2>,
    next_parser: fn(&str) -> IResult<&str, Expr>,
) -> impl Fn(&str) -> IResult<&str, Expr> {
    move |input: &str| {
        preceded(
            multispace0,
            alt((parse_binop_right(op_parser, next_parser), next_parser)),
        )(input)
    }
}

fn parse_term_n(
    op_parser: fn(&str) -> IResult<&str, ExprOp2>,
    next_parser: fn(&str) -> IResult<&str, Expr>,
) -> impl Fn(&str) -> IResult<&str, Expr> {
    move |input: &str| {
        preceded(
            multispace0,
            alt((parse_binop_none(op_parser, next_parser), next_parser)),
        )(input)
    }
}

// 1文字目は数字以外。アルファベット・アンダースコア・数字を許容
fn parse_identifier(input: &str) -> IResult<&str, &str> {
    preceded(
        multispace0,
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_")))),
        )),
    )(input)
}

// 0: 関数呼び出し・定数・括弧・ラムダ式
fn parse_int(input: &str) -> IResult<&str, Expr> {
    map(digit1, |s: &str| Expr::IVal(s.parse().unwrap()))(input)
}

fn parse_float(input: &str) -> IResult<&str, Expr> {
    map(
        separated_pair(digit1, char('.'), digit1),
        |(a, b): (&str, &str)| Expr::FVal(format!("{}.{}", a, b).parse().unwrap()),
    )(input)
}

// "e"とか"pi"とか。存在チェックは計算時にやる
fn parse_named_const(input: &str) -> IResult<&str, Expr> {
    map(parse_identifier, |s: &str| Expr::Const(s.to_owned()))(input)
}

// (x, y, z) => x + y + z
// to Lambda(["x", "y", "z"], [Op2(Add, Op2(Add, Var("x"), Var("y")), Var("z"))])
fn parse_lambda(input: &str) -> IResult<&str, Expr> {
    map(
        pair(
            delimited(
                preceded(multispace0, char('(')),
                separated_list0(char(','), preceded(multispace0, parse_identifier)),
                preceded(multispace0, char(')')),
            ),
            preceded(multispace0, preceded(tag("=>"), parse_expr)),
        ),
        |(args, body)| Expr::Lambda(args.into_iter().map(String::from).collect(), Box::new(body)),
    )(input)
}

// x => x + 1
fn parse_lambda_one(input: &str) -> IResult<&str, Expr> {
    map(
        pair(
            preceded(multispace0, parse_identifier),
            preceded(multispace0, preceded(tag("=>"), parse_expr)),
        ),
        |(arg, body)| Expr::Lambda(vec![arg.to_owned()], Box::new(body)),
    )(input)
}

fn parse_paren(input: &str) -> IResult<&str, Expr> {
    delimited(char('('), parse_expr, char(')'))(input)
}

// \n -> CR, \t -> TAB, \u{1234} -> Unicode
fn parse_special_escape(input: &str) -> IResult<&str, char> {
    preceded(
        tag("\\"),
        alt((
            value('\n', tag("n")),
            value('\t', tag("t")),
            map(
                delimited(
                    tag("u{"),
                    recognize(many1_count(one_of("0123456789abcdefABCDEF"))),
                    tag("}"),
                ),
                |s: &str| {
                    let code = u32::from_str_radix(s, 16).unwrap();
                    std::char::from_u32(code).unwrap()
                },
            ),
        )),
    )(input)
}

fn parse_escaped_char(input: &str) -> IResult<&str, char> {
    preceded(char('\\'), anychar)(input)
}

fn parse_string_content(input: &str) -> IResult<&str, String> {
    map(
        many0(alt((
            parse_special_escape,
            parse_escaped_char,
            none_of("\""),
        ))),
        |chars| chars.into_iter().collect(),
    )(input)
}

fn parse_string_literal(input: &str) -> IResult<&str, Expr> {
    map(
        delimited(char('\"'), parse_string_content, char('\"')),
        Expr::SVal,
    )(input)
}

fn parse_list_literal(input: &str) -> IResult<&str, Expr> {
    map(
        delimited(char('['), separated_list0(char(','), parse_expr), char(']')),
        |exprs| Expr::List(exprs.into_iter().collect()),
    )(input)
}

fn parse_object_literal(input: &str) -> IResult<&str, Expr> {
    map(
        delimited(
            char('{'),
            separated_list0(
                char(','),
                separated_pair(
                    parse_identifier,
                    preceded(multispace0, char(':')),
                    parse_expr,
                ),
            ),
            char('}'),
        ),
        |pairs| {
            Expr::Object(
                pairs
                    .into_iter()
                    .map(|(k, v)| (k.to_owned(), Box::new(v)))
                    .collect(),
            )
        },
    )(input)
}

fn parse_term_before_postfix(input: &str) -> IResult<&str, Expr> {
    preceded(
        multispace0,
        alt((
            parse_lambda,
            parse_lambda_one,
            parse_paren,
            parse_float,
            parse_int,
            parse_named_const,
            parse_string_literal,
            parse_list_literal,
            parse_object_literal,
        )),
    )(input)
}

#[derive(Debug, Clone)]
enum PostfixExprPart {
    Apply(Vec<Expr>),
    ListAt(Box<Expr>),
    Get(String),
}

fn parse_apply(input: &str) -> IResult<&str, PostfixExprPart> {
    map(
        delimited(
            preceded(multispace0, char('(')),
            separated_list0(char(','), preceded(multispace0, parse_expr)),
            preceded(multispace0, char(')')),
        ),
        |args| PostfixExprPart::Apply(args.into_iter().collect()),
    )(input)
}

fn parse_list_at(input: &str) -> IResult<&str, PostfixExprPart> {
    map(
        delimited(
            preceded(multispace0, char('[')),
            preceded(multispace0, parse_expr),
            preceded(multispace0, char(']')),
        ),
        |ix| PostfixExprPart::ListAt(Box::new(ix)),
    )(input)
}

fn parse_object_get(input: &str) -> IResult<&str, PostfixExprPart> {
    map(
        preceded(preceded(multispace0, char('.')), parse_identifier),
        |k| PostfixExprPart::Get(k.to_owned()),
    )(input)
}

fn postfix(expr: Expr, postfix: PostfixExprPart) -> Expr {
    match postfix {
        PostfixExprPart::Apply(args) => Expr::Apply(Box::new(expr), args.into_iter().collect()),
        PostfixExprPart::ListAt(ix) => Expr::At(Box::new(expr), ix),
        PostfixExprPart::Get(k) => Expr::Get(Box::new(expr), k),
    }
}

// postfix(ApplyとListAt)を全部処理
// 方針：parse_term_before_postfixを取ったのち、parse_postfixを取れるだけ取って、左からたたみ込んで適用
fn parse_term0(input: &str) -> IResult<&str, Expr> {
    let (input, init) = parse_term_before_postfix(input)?;
    fold_many0(
        alt((parse_apply, parse_list_at, parse_object_get)),
        move || init.clone(),
        postfix,
    )(input)
}

// 1: 単項演算子
fn parse_neg(input: &str) -> IResult<&str, Expr> {
    map(pair(char('-'), parse_term0), |(_, e)| {
        Expr::Op1(ExprOp1::Neg, Box::new(e))
    })(input)
}

fn parse_one_dice(input: &str) -> IResult<&str, Expr> {
    map(pair(one_of("dD"), parse_term0), |(_, e)| {
        Expr::Op1(ExprOp1::OneDice, Box::new(e))
    })(input)
}

fn parse_term1(input: &str) -> IResult<&str, Expr> {
    preceded(multispace0, alt((parse_neg, parse_one_dice, parse_term0)))(input)
}

// 2: D 左結合
fn parse_term2(input: &str) -> IResult<&str, Expr> {
    parse_term_l(|op| map(one_of("dD"), |_| ExprOp2::Dice)(op), parse_term1)(input)
}

// 3: ^ ** (←同義) 右結合
fn parse_term3(input: &str) -> IResult<&str, Expr> {
    parse_term_r(
        |op| map(alt((tag("^"), tag("**"))), |_| ExprOp2::Pow)(op),
        parse_term2,
    )(input)
}

// 4: * / % 左結合
fn parse_term4(input: &str) -> IResult<&str, Expr> {
    parse_term_l(
        |op| {
            alt((
                map(char('*'), |_| ExprOp2::Mul),
                map(char('/'), |_| ExprOp2::Div),
                map(char('%'), |_| ExprOp2::Mod),
            ))(op)
        },
        parse_term3,
    )(input)
}

// 5: + - 左結合
fn parse_term5(input: &str) -> IResult<&str, Expr> {
    parse_term_l(
        |op| {
            alt((
                map(char('+'), |_| ExprOp2::Add),
                map(char('-'), |_| ExprOp2::Sub),
            ))(op)
        },
        parse_term4,
    )(input)
}

// 6: > >= < <= == != 無結合
fn parse_term6(input: &str) -> IResult<&str, Expr> {
    parse_term_n(
        |op| {
            alt((
                map(tag(">="), |_| ExprOp2::Ge),
                map(tag("<="), |_| ExprOp2::Le),
                map(tag("=="), |_| ExprOp2::Eq),
                map(tag("!="), |_| ExprOp2::Ne),
                map(char('>'), |_| ExprOp2::Gt),
                map(char('<'), |_| ExprOp2::Lt),
            ))(op)
        },
        parse_term5,
    )(input)
}

// 7: && || ^^ 左結合
fn parse_term7(input: &str) -> IResult<&str, Expr> {
    parse_term_l(
        |op| {
            alt((
                map(tag("&&"), |_| ExprOp2::AndL),
                map(tag("||"), |_| ExprOp2::OrL),
                map(tag("^^"), |_| ExprOp2::XorL),
            ))(op)
        },
        parse_term6,
    )(input)
}

pub fn parse_expr(input: &str) -> IResult<&str, Expr> {
    parse_term7(input)
}

/*
-----------------------------
評価
-----------------------------
*/

type Context = DashMap<String, EvalResult>;

#[derive(Debug, Clone)]
pub enum EvalResult {
    IVal(i64),
    FVal(f64),
    BVal(bool),
    SVal(String),
    List(Vec<EvalResult>),
    Object(HashMap<String, Box<EvalResult>>),
    Closure(Vec<String>, Box<Expr>, Box<Context>),
    FuncStdLib(EvalStdLibFun),
    FuncIf,
    FuncLazy,
    Lazy(Box<Expr>), //適用を受けるまで遅延
}

impl PartialEq for EvalResult {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::IVal(i1), Self::IVal(i2)) => i1 == i2,
            (Self::FVal(f1), Self::FVal(f2)) => f1 == f2,
            (Self::BVal(b1), Self::BVal(b2)) => b1 == b2,
            (Self::SVal(s1), Self::SVal(s2)) => s1 == s2,
            (Self::List(l1), Self::List(l2)) => l1 == l2,
            (Self::Object(o1), Self::Object(o2)) => o1 == o2,
            (Self::Closure(a, b, _), Self::Closure(c, d, _)) => a == c && b == d,
            (Self::FuncStdLib(f1), Self::FuncStdLib(f2)) => f1 == f2,
            (Self::Lazy(e1), Self::Lazy(e2)) => e1 == e2,
            _ => false,
        }
    }
}

fn show_context(ctx: &Context) -> String {
    let inner = ctx
        .iter()
        .map(|e| format!("{}: {}", e.key(), e.value()))
        .collect::<Vec<String>>()
        .join(", ");
    format!("{{{}}}", inner)
}

impl std::fmt::Display for EvalResult {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::IVal(i) => write!(f, "{}", i),
            Self::FVal(v) => write!(f, "{}", v),
            Self::BVal(b) => write!(f, "{}", b),
            Self::SVal(s) => write!(f, "\"{}\"", s),
            Self::List(l) => write!(
                f,
                "[{}]",
                l.iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Object(o) => write!(
                f,
                "{{{}}}",
                o.iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Closure(params, body, context) => write!(
                f,
                "({} => {})@{}",
                params.join(", "),
                body,
                show_context(context)
            ),
            Self::FuncStdLib(fun) => write!(f, "{}", fun),
            Self::FuncIf => write!(f, "if"),
            Self::FuncLazy => write!(f, "lazy"),
            Self::Lazy(body) => write!(f, "Lazy({})", body),
        }
    }
}

#[derive(Debug, Clone)]
pub enum EvalError {
    NegativeDice,
    InvalidDice,
    TooManyDice,
    UndefinedVar(String),
    //MismatchedType(ResultType, ResultType),
    ArgCountMismatch(usize, usize),
    StepLimitExceeded,
    NotANumber(EvalResult),
    NotAFunction(EvalResult),
    NotAList(EvalResult),
    NotAnIndex(EvalResult),
    NotAnObject(EvalResult),
    OutOfRange,
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::NegativeDice => write!(f, "Negative dice"),
            Self::InvalidDice => write!(f, "Invalid dice"),
            Self::TooManyDice => write!(f, "Too many dice"),
            Self::UndefinedVar(s) => write!(f, "Undefined variable: {}", s),
            Self::ArgCountMismatch(a, b) => {
                write!(f, "Argument count mismatch: {} and {}", a, b)
            }
            Self::StepLimitExceeded => write!(f, "Step limit exceeded"),
            Self::OutOfRange => write!(f, "Out of range"),
            Self::NotANumber(e) => write!(f, "{} is not a number", e),
            Self::NotAFunction(e) => write!(f, "{} is not a function", e),
            Self::NotAList(e) => write!(f, "{} is not a list", e),
            Self::NotAnIndex(e) => write!(f, "{} is not an index", e),
            Self::NotAnObject(e) => write!(f, "{} is not an object", e),
        }
    }
}

pub fn error_str((e, expr): (EvalError, Expr)) -> String {
    format!("Error: {} at {}", e, expr)
}

pub const fn val_as_float(val: &EvalResult) -> Option<f64> {
    match val {
        EvalResult::IVal(i) => Some(*i as f64),
        EvalResult::BVal(b) => Some(if *b { 1.0 } else { 0.0 }),
        EvalResult::FVal(f) => Some(*f),
        _ => None,
    }
}

pub const fn val_as_int(val: &EvalResult) -> Option<i64> {
    match val {
        EvalResult::IVal(i) => Some(*i),
        EvalResult::BVal(b) => Some(if *b { 1 } else { 0 }),
        EvalResult::FVal(f) => Some(*f as i64),
        _ => None,
    }
}

pub const fn val_as_precise_int(val: &EvalResult) -> Option<i64> {
    match val {
        EvalResult::IVal(i) => Some(*i),
        EvalResult::BVal(b) => Some(if *b { 1 } else { 0 }),
        _ => None,
    }
}

pub fn val_as_bool(val: &EvalResult) -> Option<bool> {
    match val {
        EvalResult::IVal(i) => Some(*i != 0),
        EvalResult::BVal(b) => Some(*b),
        EvalResult::FVal(f) => Some(*f != 0.0),
        _ => None,
    }
}

pub fn val_as_list(val: &EvalResult) -> Option<Vec<EvalResult>> {
    match val {
        EvalResult::List(l) => Some(l.clone()),
        EvalResult::SVal(s) => Some(s.chars().map(|c| EvalResult::SVal(c.to_string())).collect()),
        _ => None,
    }
}

fn val_numop2_if<F, G>(
    expr: &Expr,
    step: usize,
    val1: &EvalResult,
    val2: &EvalResult,
    intver: F,
    floatver: G,
) -> Result<(EvalResult, usize), (EvalError, Expr)>
where
    F: Fn(i64, i64) -> i64,
    G: Fn(f64, f64) -> f64,
{
    match (val_as_precise_int(val1), val_as_precise_int(val2)) {
        (Some(i1), Some(i2)) => Ok((EvalResult::IVal(intver(i1, i2)), step + 1)),
        _ => match (val_as_float(val1), val_as_float(val2)) {
            (Some(f1), Some(f2)) => Ok((EvalResult::FVal(floatver(f1, f2)), step + 1)),
            (Some(_), _) => Err((EvalError::NotANumber(val2.clone()), expr.clone())),
            _ => Err((EvalError::NotANumber(val1.clone()), expr.clone())),
        },
    }
}

fn val_numop2_f<F>(
    expr: &Expr,
    step: usize,
    val1: &EvalResult,
    val2: &EvalResult,
    floatver: F,
) -> Result<(EvalResult, usize), (EvalError, Expr)>
where
    F: Fn(f64, f64) -> f64,
{
    match (val_as_float(val1), val_as_float(val2)) {
        (Some(f1), Some(f2)) => Ok((EvalResult::FVal(floatver(f1, f2)), step + 1)),
        (Some(_), _) => Err((EvalError::NotANumber(val2.clone()), expr.clone())),
        _ => Err((EvalError::NotANumber(val1.clone()), expr.clone())),
    }
}

fn is_str(s: EvalResult) -> bool {
    matches!(s, EvalResult::SVal(_))
}

pub fn val_as_str(s: &EvalResult) -> String {
    match s {
        EvalResult::SVal(s) => s.clone(),
        _ => format!("{}", s),
    }
}

fn list_free_var(expr: &Expr) -> HashSet<String> {
    let result = match expr {
        Expr::IVal(_) => HashSet::new(),
        Expr::FVal(_) => HashSet::new(),
        Expr::BVal(_) => HashSet::new(),
        Expr::SVal(_) => HashSet::new(),
        Expr::List(l) => l
            .iter()
            .map(list_free_var)
            .fold(HashSet::new(), |acc, x| acc.union(&x).cloned().collect()),
        Expr::Object(o) => o
            .iter()
            .map(|(_, v)| list_free_var(v))
            .fold(HashSet::new(), |acc, x| acc.union(&x).cloned().collect()),
        Expr::Get(e, _) => list_free_var(e),
        Expr::At(e1, e2) => list_free_var(e1)
            .union(&list_free_var(e2))
            .cloned()
            .collect(),
        Expr::Const(s) => {
            let mut set = HashSet::new();
            set.insert(s.clone());
            set
        }
        Expr::Op1(_, e) => list_free_var(e),
        Expr::Op2(_, e1, e2) => list_free_var(e1)
            .union(&list_free_var(e2))
            .cloned()
            .collect(),
        Expr::Apply(f, args) => list_free_var(f)
            .union(
                &args
                    .iter()
                    .map(list_free_var)
                    .fold(HashSet::new(), |acc, x| acc.union(&x).cloned().collect()),
            )
            .cloned()
            .collect(),
        Expr::Lambda(params, body) => list_free_var(body)
            .difference(&params.iter().cloned().collect())
            .cloned()
            .collect(),
    };
    //println!("list_free_var: {}, result: {:?}", expr, result);
    result
}

const STEP_LIMIT: usize = 10000;

fn eval_expr_ctx(
    expr: &Expr,
    step: usize,
    force_eval: bool,
    global_context: &Context,
    local_context: &Context,
) -> Result<(EvalResult, usize), (EvalError, Expr)> {
    if step > STEP_LIMIT {
        return Err((EvalError::StepLimitExceeded, expr.clone()));
    }
    let result = match expr {
        Expr::IVal(i) => Ok((EvalResult::IVal(*i), step)),
        Expr::FVal(f) => Ok((EvalResult::FVal(*f), step)),
        Expr::BVal(b) => Ok((EvalResult::BVal(*b), step)),
        Expr::SVal(s) => Ok((EvalResult::SVal(s.clone()), step)),
        Expr::List(l) => {
            let mut new_list = Vec::new();
            let mut steps = step + 1;
            for e in l {
                let (val, next_step) =
                    eval_expr_ctx(e, steps, false, global_context, local_context)?;
                new_list.push(val);
                steps = next_step;
            }
            Ok((EvalResult::List(new_list), steps))
        }
        Expr::Object(o) => {
            let mut new_obj = HashMap::new();
            let mut steps = step + 1;
            for (k, v) in o {
                let (val, next_step) =
                    eval_expr_ctx(v, steps, false, global_context, local_context)?;
                new_obj.insert(k.clone(), Box::new(val));
                steps = next_step;
            }
            Ok((EvalResult::Object(new_obj), steps))
        }
        Expr::Get(e, k) => {
            let (val, next_step) = eval_expr_ctx(e, step + 1, true, global_context, local_context)?;
            match val {
                EvalResult::Object(o) => o.get(k).map_or_else(
                    || Err((EvalError::UndefinedVar(k.clone()), expr.clone())),
                    |result| Ok((*result.clone(), next_step + 1)),
                ),
                _ => Err((EvalError::NotAnObject(val), expr.clone())),
            }
        }

        Expr::At(e1, e2) => {
            let (val1, next_step) =
                eval_expr_ctx(e1, step + 1, true, global_context, local_context)?;
            let (val2, next_step) =
                eval_expr_ctx(e2, next_step + 1, true, global_context, local_context)?;
            match val_as_list(&val1) {
                Some(v) => {
                    let index = match val_as_int(&val2) {
                        Some(i) => i as isize,
                        _ => return Err((EvalError::NotAnIndex(val2), expr.clone())),
                    };
                    if index < 0 || index >= v.len() as isize {
                        return Err((EvalError::OutOfRange, expr.clone()));
                    }
                    Ok((v[index as usize].clone(), next_step + 1))
                }
                _ => Err((EvalError::NotAList(val1), expr.clone())),
            }
        }

        Expr::Const(s) => local_context.get(s).map_or_else(
            || {
                global_context.get(s).map_or_else(
                    || {
                        match_const(s).map_or_else(
                            || Err((EvalError::UndefinedVar(s.clone()), expr.clone())),
                            |result| Ok((result, step)),
                        )
                    },
                    |result| Ok((result.clone(), step)),
                )
            },
            |result| Ok((result.clone(), step)),
        ),
        Expr::Op1(op, e) => {
            let (val, next_step) = eval_expr_ctx(e, step + 1, true, global_context, local_context)?;
            let fval = match val {
                EvalResult::IVal(i) => i as f64,
                EvalResult::FVal(f) => f,
                _ => return Err((EvalError::NotANumber(val), expr.clone())),
            };

            match op {
                ExprOp1::Neg => Ok((EvalResult::FVal(-fval), next_step + 1)),
                ExprOp1::OneDice => {
                    let i = fval as i64;
                    if i < 1 {
                        return Err((EvalError::InvalidDice, expr.clone()));
                    }
                    let r = rand::thread_rng().gen_range(1..=i);
                    Ok((EvalResult::IVal(r), next_step + 1))
                }
                ExprOp1::NotL => Ok((EvalResult::BVal(fval == 0.0), next_step + 1)),
            }
        }
        Expr::Op2(op, e1, e2) => {
            let (val1, next_step) =
                eval_expr_ctx(e1, step + 1, true, global_context, local_context)?;
            let (val2, next_step) =
                eval_expr_ctx(e2, next_step + 1, true, global_context, local_context)?;

            let shortcircuit: Option<Result<(EvalResult, usize), (EvalError, Expr)>> = match op {
                ExprOp2::Add => {
                    if is_str(val1.clone()) || is_str(val2.clone()) {
                        let str1 = match val1.clone() {
                            EvalResult::SVal(s) => s,
                            _ => format!("{}", val1),
                        };
                        let str2 = match val2.clone() {
                            EvalResult::SVal(s) => s,
                            _ => format!("{}", val2),
                        };
                        Some(Ok((
                            EvalResult::SVal(format!("{}{}", str1, str2)),
                            next_step + 1,
                        )))
                    } else {
                        match (val1.clone(), val2.clone()) {
                            (EvalResult::List(l1), EvalResult::List(l2)) => {
                                let mut new_list = Vec::new();
                                for e in l1 {
                                    new_list.push(e.clone());
                                }
                                for e in l2 {
                                    new_list.push(e.clone());
                                }
                                Some(Ok((EvalResult::List(new_list), next_step + 1)))
                            }
                            _ => None,
                        }
                    }
                }
                _ => None,
            };

            match shortcircuit {
                Some(Ok(v)) => Ok(v),
                Some(Err(e)) => Err(e),
                None => {
                    let Some(fval1) = val_as_float(&val1) else {
                        return Err((EvalError::NotANumber(val1), expr.clone()));
                    };

                    let Some(fval2) = val_as_float(&val2) else {
                        return Err((EvalError::NotANumber(val2), expr.clone()));
                    };

                    let bval1 = fval1 != 0.0;
                    let bval2 = fval2 != 0.0;

                    match op {
                        ExprOp2::Add => val_numop2_if(
                            expr,
                            step,
                            &val1,
                            &val2,
                            |i1, i2| i1 + i2,
                            |f1, f2| f1 + f2,
                        ),
                        ExprOp2::Sub => val_numop2_if(
                            expr,
                            step,
                            &val1,
                            &val2,
                            |i1, i2| i1 - i2,
                            |f1, f2| f1 - f2,
                        ),
                        ExprOp2::Mul => val_numop2_if(
                            expr,
                            step,
                            &val1,
                            &val2,
                            |i1, i2| i1 * i2,
                            |f1, f2| f1 * f2,
                        ),
                        ExprOp2::Mod => val_numop2_f(expr, step, &val1, &val2, |f1, f2| f1 % f2),
                        ExprOp2::Pow => val_numop2_f(expr, step, &val1, &val2, f64::powf),
                        ExprOp2::Div => val_numop2_f(expr, step, &val1, &val2, |f1, f2| f1 / f2),
                        ExprOp2::Dice => {
                            let num = fval1 as i64;
                            let size = fval2 as i64;
                            if num < 0 {
                                return Err((EvalError::NegativeDice, expr.clone()));
                            }
                            if size < 1 {
                                return Err((EvalError::InvalidDice, expr.clone()));
                            }
                            if num > 10000 {
                                return Err((EvalError::TooManyDice, expr.clone()));
                            }
                            let mut sum = 0;
                            for _ in 0..num {
                                sum += rand::thread_rng().gen_range(1..=size);
                            }
                            Ok((EvalResult::IVal(sum), next_step + 1))
                        }
                        ExprOp2::Gt => Ok((EvalResult::BVal(fval1 > fval2), next_step + 1)),
                        ExprOp2::Ge => Ok((EvalResult::BVal(fval1 >= fval2), next_step + 1)),
                        ExprOp2::Lt => Ok((EvalResult::BVal(fval1 < fval2), next_step + 1)),
                        ExprOp2::Le => Ok((EvalResult::BVal(fval1 <= fval2), next_step + 1)),
                        ExprOp2::Eq => Ok((EvalResult::BVal(fval1 == fval2), next_step + 1)),
                        ExprOp2::Ne => Ok((EvalResult::BVal(fval1 != fval2), next_step + 1)),

                        ExprOp2::AndL => Ok((EvalResult::BVal(bval1 && bval2), next_step + 1)),
                        ExprOp2::OrL => Ok((EvalResult::BVal(bval1 || bval2), next_step + 1)),
                        ExprOp2::XorL => Ok((EvalResult::BVal(bval1 ^ bval2), next_step + 1)),
                    }
                }
            }
        }
        Expr::Apply(fun, args) => {
            let (vfun, next_step) =
                eval_expr_ctx(fun, step + 1, true, global_context, local_context)?;

            let shortcircuit = match vfun.clone() {
                EvalResult::FuncIf => {
                    if args.len() != 3 {
                        return Err((EvalError::ArgCountMismatch(args.len(), 3), expr.clone()));
                    }
                    let (cond, next_step) =
                        eval_expr_ctx(&args[0], next_step, true, global_context, local_context)?;
                    let Some(bval) = val_as_bool(&cond) else {
                        return Err((EvalError::NotANumber(cond), expr.clone()));
                    };
                    let (val, next_step) = if bval {
                        eval_expr_ctx(&args[1], next_step, false, global_context, local_context)?
                    } else {
                        eval_expr_ctx(&args[2], next_step, false, global_context, local_context)?
                    };
                    Some(Ok((val, next_step)))
                }
                EvalResult::FuncLazy => {
                    if args.len() != 1 {
                        return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                    }
                    Some(Ok((EvalResult::Lazy(Box::new(args[0].clone())), next_step)))
                }
                EvalResult::Lazy(body) => {
                    let newexpr = &Expr::Apply(body, args.clone());
                    Some(eval_expr_ctx(
                        newexpr,
                        next_step,
                        true,
                        global_context,
                        local_context,
                    ))
                }
                _ => None,
            };

            match shortcircuit {
                Some(Ok(v)) => Ok(v),
                Some(Err(e)) => Err(e),
                None => {
                    let mut vargs = Vec::new();
                    let mut steps = next_step;
                    for e in args {
                        let (val, next_step) =
                            eval_expr_ctx(e, steps, false, global_context, local_context)?;
                        vargs.push(val);
                        steps = next_step;
                    }
                    eval_apply(expr, steps, global_context, local_context, vfun, vargs)
                }
            }
        }
        Expr::Lambda(name, body) => {
            let free_vars = list_free_var(body);
            let captured = DashMap::new();
            for varname in free_vars {
                if let Some(val) = local_context.get(&varname) {
                    captured.insert(varname, val.clone());
                }
            }

            Ok((
                EvalResult::Closure(name.clone(), body.clone(), Box::new(captured)),
                step,
            ))
        }
    };

    if force_eval {
        match result {
            Ok((EvalResult::Lazy(e), next_step)) => {
                eval_expr_ctx(&e, next_step, true, global_context, local_context)
            }
            _ => result,
        }
    } else {
        result
    }
}

pub fn eval_apply(
    expr: &Expr,
    steps: usize,
    global_context: &Context,
    local_context: &Context,
    func: EvalResult,
    args: Vec<EvalResult>,
) -> Result<(EvalResult, usize), (EvalError, Expr)> {
    if steps > STEP_LIMIT {
        return Err((EvalError::StepLimitExceeded, expr.clone()));
    }

    match func {
        EvalResult::Closure(params, body, ctx) => {
            if args.len() != params.len() {
                return Err((
                    EvalError::ArgCountMismatch(args.len(), params.len()),
                    expr.clone(),
                ));
            }

            let new_context = ctx;
            for (param, argval) in params.iter().zip(args.iter()) {
                new_context.insert(param.clone(), argval.clone());
            }
            let steps = steps + 1;
            eval_expr_ctx(&body, steps, false, global_context, &new_context)
        }
        EvalResult::FuncStdLib(libfun) => {
            let steps = steps + 1;
            eval_stdlib(expr, steps, global_context, local_context, libfun, args)
        }
        _ => Err((EvalError::NotAFunction(func), expr.clone())),
    }
}

#[allow(clippy::cognitive_complexity)]
pub fn eval_stdlib(
    expr: &Expr,
    step: usize,
    global_context: &Context,
    local_context: &Context,
    func: EvalStdLibFun,
    args: Vec<EvalResult>,
) -> Result<(EvalResult, usize), (EvalError, Expr)> {
    let libfun: LibFun = get_libfun(func);
    (libfun.body)(expr, step, global_context, local_context, args)
}

pub fn get_libfun(func: EvalStdLibFun) -> LibFun {
    match func {
        EvalStdLibFun::Sin => LibFun {
            name: "sin".to_owned(),
            alias: vec![],
            usage: "`sin(x)`".to_owned(),
            note: "".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                val_as_float(&args[0]).map_or_else(
                    || Err((EvalError::NotANumber(args[0].clone()), expr.clone())),
                    |f| Ok((EvalResult::FVal(f.sin()), step + 1)),
                )
            }),
        },
        EvalStdLibFun::Cos => LibFun {
            name: "cos".to_owned(),
            alias: vec![],
            usage: "`cos(x)`".to_owned(),
            note: "".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                val_as_float(&args[0]).map_or_else(
                    || Err((EvalError::NotANumber(args[0].clone()), expr.clone())),
                    |f| Ok((EvalResult::FVal(f.cos()), step + 1)),
                )
            }),
        },
        EvalStdLibFun::Tan => LibFun {
            name: "tan".to_owned(),
            alias: vec![],
            usage: "`tan(x)`".to_owned(),
            note: "".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                val_as_float(&args[0]).map_or_else(
                    || Err((EvalError::NotANumber(args[0].clone()), expr.clone())),
                    |f| Ok((EvalResult::FVal(f.tan()), step + 1)),
                )
            }),
        },
        EvalStdLibFun::LogE => LibFun {
            name: "ln".to_owned(),
            alias: vec!["loge".to_owned(), "logE".to_owned()],
            usage: "`ln(x)`".to_owned(),
            note: "".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                val_as_float(&args[0]).map_or_else(
                    || Err((EvalError::NotANumber(args[0].clone()), expr.clone())),
                    |f| Ok((EvalResult::FVal(f.ln()), step + 1)),
                )
            }),
        },
        EvalStdLibFun::Log10 => LibFun {
            name: "log10".to_owned(),
            alias: vec!["log".to_owned()],
            usage: "`log10(x)`".to_owned(),
            note: "".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                val_as_float(&args[0]).map_or_else(
                    || Err((EvalError::NotANumber(args[0].clone()), expr.clone())),
                    |f| Ok((EvalResult::FVal(f.log10()), step + 1)),
                )
            }),
        },
        EvalStdLibFun::Log2 => LibFun {
            name: "log2".to_owned(),
            alias: vec!["lg".to_owned(), "lb".to_owned()],
            usage: "`log2(x)`".to_owned(),
            note: "".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                val_as_float(&args[0]).map_or_else(
                    || Err((EvalError::NotANumber(args[0].clone()), expr.clone())),
                    |f| Ok((EvalResult::FVal(f.log2()), step + 1)),
                )
            }),
        },
        EvalStdLibFun::Abs => LibFun {
            name: "abs".to_owned(),
            alias: vec![],
            usage: "`abs(x)`".to_owned(),
            note: "".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                val_as_precise_int(&args[0]).map_or_else(
                    || {
                        val_as_float(&args[0]).map_or_else(
                            || Err((EvalError::NotANumber(args[0].clone()), expr.clone())),
                            |f| Ok((EvalResult::FVal(f.abs()), step + 1)),
                        )
                    },
                    |i| Ok((EvalResult::IVal(i.abs()), step + 1)),
                )
            }),
        },
        EvalStdLibFun::Floor => LibFun {
            name: "floor".to_owned(),
            alias: vec![],
            usage: "`floor(x)`".to_owned(),
            note: "".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                val_as_float(&args[0]).map_or_else(
                    || Err((EvalError::NotANumber(args[0].clone()), expr.clone())),
                    |f| Ok((EvalResult::IVal(f.floor() as i64), step + 1)),
                )
            }),
        },
        EvalStdLibFun::Ceil => LibFun {
            name: "ceil".to_owned(),
            alias: vec![],
            usage: "`ceil(x)`".to_owned(),
            note: "".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                val_as_float(&args[0]).map_or_else(
                    || Err((EvalError::NotANumber(args[0].clone()), expr.clone())),
                    |f| Ok((EvalResult::IVal(f.ceil() as i64), step + 1)),
                )
            }),
        },
        EvalStdLibFun::Round => LibFun {
            name: "round".to_owned(),
            alias: vec![],
            usage: "`round(x)`".to_owned(),
            note: "".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                val_as_float(&args[0]).map_or_else(
                    || Err((EvalError::NotANumber(args[0].clone()), expr.clone())),
                    |f| Ok((EvalResult::IVal(f.round() as i64), step + 1)),
                )
            }),
        },
        EvalStdLibFun::URand => LibFun {
            name: "urand".to_owned(),
            alias: vec![],
            usage: "`urand()`".to_owned(),
            note: "0~1の一様乱数を生成します".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.is_empty() {
                    Ok((
                        EvalResult::FVal(rand::thread_rng().gen_range(0.0..1.0)),
                        step + 1,
                    ))
                } else {
                    Err((EvalError::ArgCountMismatch(args.len(), 0), expr.clone()))
                }
            }),
        },
        EvalStdLibFun::GRand => LibFun {
            name: "grand".to_owned(),
            alias: vec![],
            usage: "`grand()`".to_owned(),
            note: "標準正規分布に従う乱数を生成します".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.is_empty() {
                    Ok((
                        EvalResult::FVal(StandardNormal.sample(&mut rand::thread_rng())),
                        step + 1,
                    ))
                } else {
                    Err((EvalError::ArgCountMismatch(args.len(), 0), expr.clone()))
                }
            }),
        },
        EvalStdLibFun::Map => LibFun {
            name: "map".to_owned(),
            alias: vec![],
            usage: "`map(f, list)`".to_owned(),
            note: "リストの各要素に関数を適用します".to_owned(),
            body: Box::new(|expr, step, global_context, local_context, args| {
                if args.len() != 2 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 2), expr.clone()));
                }
                match val_as_list(&args[1]) {
                    Some(l) => {
                        let mut new_list = Vec::new();
                        let mut step = step + 1;
                        for e in l {
                            let (val, next_step) = eval_apply(
                                expr,
                                step + 1,
                                global_context,
                                local_context,
                                args[0].clone(),
                                vec![e.clone()],
                            )?;
                            step = next_step;
                            new_list.push(val);
                        }
                        Ok((EvalResult::List(new_list), step))
                    }
                    _ => Err((EvalError::NotAList(args[1].clone()), expr.clone())),
                }
            }),
        },
        EvalStdLibFun::Geni => LibFun {
            name: "geni".to_owned(),
            alias: vec!["generatei".to_owned()],
            usage: "`geni(f, n)`".to_owned(),
            note: "fに0~(n-1)を適用した結果を要素とするリストを生成します".to_owned(),
            body: Box::new(|expr, step, global_context, local_context, args| {
                if args.len() != 2 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 2), expr.clone()));
                }
                match val_as_int(&args[1]) {
                    Some(n) => {
                        let mut new_list = Vec::new();
                        let mut step = step + 1;
                        for i in 0..n {
                            let (val, next_step) = eval_apply(
                                expr,
                                step + 1,
                                global_context,
                                local_context,
                                args[0].clone(),
                                vec![EvalResult::IVal(i)],
                            )?;
                            step = next_step;
                            new_list.push(val);
                        }
                        Ok((EvalResult::List(new_list), step))
                    }
                    _ => Err((EvalError::NotANumber(args[1].clone()), expr.clone())),
                }
            }),
        },
        EvalStdLibFun::Repeat => LibFun {
            name: "repeat".to_owned(),
            alias: vec![],
            usage: "`repeat(f, n)`".to_owned(),
            note: "fの結果をn個含むリストを生成します".to_owned(),
            body: Box::new(|expr, step, global_context, local_context, args| {
                if args.len() != 2 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 2), expr.clone()));
                }
                match val_as_int(&args[1]) {
                    Some(n) => {
                        let mut new_list = Vec::new();
                        let mut step = step + 1;
                        for _ in 0..n {
                            let (val, next_step) = eval_apply(
                                expr,
                                step + 1,
                                global_context,
                                local_context,
                                args[0].clone(),
                                vec![],
                            )?;
                            step = next_step;
                            new_list.push(val);
                        }
                        Ok((EvalResult::List(new_list), step))
                    }
                    _ => Err((EvalError::NotANumber(args[1].clone()), expr.clone())),
                }
            }),
        },
        EvalStdLibFun::Filter => LibFun {
            name: "filter".to_owned(),
            alias: vec![],
            usage: "`filter(f, list)`".to_owned(),
            note: "fがtruthyな値を返す要素のみを含むリストを生成します".to_owned(),
            body: Box::new(|expr, step, global_context, local_context, args| {
                if args.len() != 2 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 2), expr.clone()));
                }
                match val_as_list(&args[1]) {
                    Some(l) => {
                        let mut new_list = Vec::new();
                        for e in l {
                            let (val, _) = eval_apply(
                                expr,
                                step + 1,
                                global_context,
                                local_context,
                                args[0].clone(),
                                vec![e.clone()],
                            )?;
                            if val_as_bool(&val) == Some(true) {
                                new_list.push(e.clone());
                            }
                        }
                        Ok((EvalResult::List(new_list), step + 1))
                    }
                    _ => Err((EvalError::NotAList(args[1].clone()), expr.clone())),
                }
            }),
        },
        EvalStdLibFun::ZipWith => LibFun {
            name: "zipWith".to_owned(),
            alias: vec![],
            usage: "`zipWith(f, list1, list2)`".to_owned(),
            note: "fをlist1とlist2の対応する要素に適用した結果を要素とするリストを生成します"
                .to_owned(),
            body: Box::new(|expr, step, global_context, local_context, args| {
                if args.len() != 3 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 3), expr.clone()));
                }
                match (val_as_list(&args[1]), val_as_list(&args[2])) {
                    (Some(l1), Some(l2)) => {
                        let mut new_list = Vec::new();
                        let mut step = step + 1;
                        for (e1, e2) in l1.iter().zip(l2.iter()) {
                            let (val, next_step) = eval_apply(
                                expr,
                                step + 1,
                                global_context,
                                local_context,
                                args[0].clone(),
                                vec![e1.clone(), e2.clone()],
                            )?;
                            step = next_step;
                            new_list.push(val);
                        }
                        Ok((EvalResult::List(new_list), step))
                    }
                    (Some(_), _) => Err((EvalError::NotAList(args[1].clone()), expr.clone())),
                    _ => Err((EvalError::NotAList(args[0].clone()), expr.clone())),
                }
            }),
        },
        EvalStdLibFun::Foldl => LibFun {
            name: "foldl".to_owned(),
            alias: vec![],
            usage: "`foldl(f, init, list)`".to_owned(),
            note: "initを初期値としてlistをfで左から畳み込みます".to_owned(),
            body: Box::new(|expr, step, global_context, local_context, args| {
                if args.len() != 3 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 3), expr.clone()));
                }
                match val_as_list(&args[2]) {
                    Some(l) => {
                        let mut acc = args[1].clone();
                        let mut step = step + 1;
                        for e in l {
                            let (val, next_step) = eval_apply(
                                expr,
                                step,
                                global_context,
                                local_context,
                                args[0].clone(),
                                vec![acc, e.clone()],
                            )?;
                            acc = val;
                            step = next_step;
                        }
                        Ok((acc, step + 1))
                    }
                    _ => Err((EvalError::NotAList(args[2].clone()), expr.clone())),
                }
            }),
        },
        EvalStdLibFun::Foldr => LibFun {
            name: "foldr".to_owned(),
            alias: vec![],
            usage: "`foldr(f, init, list)`".to_owned(),
            note: "initを初期値としてlistをfで右から畳み込みます".to_owned(),
            body: Box::new(|expr, step, global_context, local_context, args| {
                if args.len() != 3 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 3), expr.clone()));
                }
                match val_as_list(&args[2]) {
                    Some(l) => {
                        let mut acc = args[1].clone();
                        let mut step = step + 1;
                        for e in l.iter().rev() {
                            let (val, next_step) = eval_apply(
                                expr,
                                step,
                                global_context,
                                local_context,
                                args[0].clone(),
                                vec![e.clone(), acc],
                            )?;
                            acc = val;
                            step = next_step;
                        }
                        Ok((acc, step + 1))
                    }
                    _ => Err((EvalError::NotAList(args[2].clone()), expr.clone())),
                }
            }),
        },
        EvalStdLibFun::Range => LibFun {
            name: "range".to_owned(),
            alias: vec![],
            usage: "`range(end)` or `range(start, end)` or `range(start, end, step)`".to_owned(),
            note: "startからendの手前までstep刻みのリストを生成します".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                let (start, stop, stepsize): (i64, i64, i64) = match args.len() {
                    1 => match val_as_int(&args[0]) {
                        Some(e) => (0, e, 1),
                        _ => return Err((EvalError::NotANumber(args[0].clone()), expr.clone())),
                    },
                    2 => match (val_as_int(&args[0]), val_as_int(&args[1])) {
                        (Some(s), Some(e)) => (s, e, 1),
                        (Some(_), _) => {
                            return Err((EvalError::NotANumber(args[1].clone()), expr.clone()))
                        }
                        _ => return Err((EvalError::NotANumber(args[0].clone()), expr.clone())),
                    },
                    3 => match (
                        val_as_int(&args[0]),
                        val_as_int(&args[1]),
                        val_as_int(&args[2]),
                    ) {
                        (Some(s), Some(e), Some(p)) => (s, e, p),
                        (Some(_), Some(_), _) => {
                            return Err((EvalError::NotANumber(args[2].clone()), expr.clone()))
                        }
                        (Some(_), _, _) => {
                            return Err((EvalError::NotANumber(args[1].clone()), expr.clone()))
                        }
                        _ => return Err((EvalError::NotANumber(args[0].clone()), expr.clone())),
                    },
                    _ => return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone())),
                };

                let mut new_list = Vec::new();
                if step > 0 {
                    for i in (start..stop).step_by(stepsize as usize) {
                        new_list.push(EvalResult::IVal(i));
                    }
                } else {
                    for i in (start..stop)
                        .rev()
                        .step_by(stepsize.unsigned_abs() as usize)
                    {
                        new_list.push(EvalResult::IVal(i));
                    }
                }
                Ok((EvalResult::List(new_list), step + 1))
            }),
        },
        EvalStdLibFun::Join => LibFun {
            name: "join".to_owned(),
            alias: vec![],
            usage: "`join(list, sep)`".to_owned(),
            note: "listの各要素をsepで結合した文字列を生成します".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 2 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 2), expr.clone()));
                }
                let sep = val_as_str(&args[1]);
                val_as_list(&args[0]).map_or_else(
                    || Err((EvalError::NotAList(args[0].clone()), expr.clone())),
                    |l| {
                        let mut new_str = String::new();
                        for (i, e) in l.iter().enumerate() {
                            if i > 0 {
                                new_str.push_str(&sep);
                            }
                            new_str.push_str(&val_as_str(e));
                        }
                        Ok((EvalResult::SVal(new_str), step + 1))
                    },
                )
            }),
        },
        EvalStdLibFun::Slice => LibFun {
            name: "slice".to_owned(),
            alias: vec![],
            usage: "`slice(list, start, end)`".to_owned(),
            note: "listのstartからendの手前までの要素を含むリストを生成します".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 3 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 3), expr.clone()));
                }
                match (
                    val_as_list(&args[0]),
                    val_as_int(&args[1]),
                    val_as_int(&args[2]),
                ) {
                    (Some(l), Some(s), Some(e)) => {
                        let start = if s < 0 { l.len() as i64 + s } else { s };
                        let end = if e < 0 { l.len() as i64 + e } else { e };
                        if start < 0 || end < 0 || start > end || end as usize > l.len() {
                            return Err((EvalError::OutOfRange, expr.clone()));
                        }
                        let mut new_list = Vec::new();
                        for i in start..end {
                            new_list.push(l[i as usize].clone());
                        }
                        Ok((EvalResult::List(new_list), step + 1))
                    }
                    (Some(_), _, _) => Err((EvalError::NotANumber(args[1].clone()), expr.clone())),
                    (_, Some(_), _) => Err((EvalError::NotANumber(args[2].clone()), expr.clone())),
                    _ => Err((EvalError::NotAList(args[0].clone()), expr.clone())),
                }
            }),
        },
        EvalStdLibFun::Len => LibFun {
            name: "len".to_owned(),
            alias: vec!["length".to_owned()],
            usage: "`len(list)`".to_owned(),
            note: "listの要素数もしくは文字数を返します".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                val_as_list(&args[0]).map_or_else(
                    || Err((EvalError::NotAList(args[0].clone()), expr.clone())),
                    |l| Ok((EvalResult::IVal(l.len() as i64), step + 1)),
                )
            }),
        },
        EvalStdLibFun::Head => LibFun {
            name: "head".to_owned(),
            alias: vec![],
            usage: "`head(list)`".to_owned(),
            note: "listの先頭要素を返します".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                val_as_list(&args[0]).map_or_else(
                    || Err((EvalError::NotAList(args[0].clone()), expr.clone())),
                    |l| {
                        if l.is_empty() {
                            Err((EvalError::OutOfRange, expr.clone()))
                        } else {
                            Ok((l[0].clone(), step + 1))
                        }
                    },
                )
            }),
        },
        EvalStdLibFun::Tail => LibFun {
            name: "tail".to_owned(),
            alias: vec![],
            usage: "`tail(list)`".to_owned(),
            note: "listの先頭要素を除いたリストを返します".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                val_as_list(&args[0]).map_or_else(
                    || Err((EvalError::NotAList(args[0].clone()), expr.clone())),
                    |l| {
                        if l.is_empty() {
                            Err((EvalError::OutOfRange, expr.clone()))
                        } else {
                            let mut new_list = Vec::new();
                            for e in l.iter().skip(1) {
                                new_list.push(e.clone());
                            }
                            Ok((EvalResult::List(new_list), step + 1))
                        }
                    },
                )
            }),
        },
        EvalStdLibFun::Last => LibFun {
            name: "last".to_owned(),
            alias: vec![],
            usage: "`last(list)`".to_owned(),
            note: "listの最後の要素を返します".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                val_as_list(&args[0]).map_or_else(
                    || Err((EvalError::NotAList(args[0].clone()), expr.clone())),
                    |l| {
                        if l.is_empty() {
                            Err((EvalError::OutOfRange, expr.clone()))
                        } else {
                            Ok((l.last().unwrap().clone(), step + 1))
                        }
                    },
                )
            }),
        },
        EvalStdLibFun::Init => LibFun {
            name: "init".to_owned(),
            alias: vec![],
            usage: "`init(list)`".to_owned(),
            note: "listの最後の要素を除いたリストを返します".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                val_as_list(&args[0]).map_or_else(
                    || Err((EvalError::NotAList(args[0].clone()), expr.clone())),
                    |l| {
                        if l.is_empty() {
                            Err((EvalError::OutOfRange, expr.clone()))
                        } else {
                            let mut new_list = Vec::new();
                            for e in l.iter().take(l.len() - 1) {
                                new_list.push(e.clone());
                            }
                            Ok((EvalResult::List(new_list), step + 1))
                        }
                    },
                )
            }),
        },
        EvalStdLibFun::While => LibFun {
            name: "while".to_owned(),
            alias: vec![],
            usage: "`while(cond, body, init)`".to_owned(),
            note: "condがtrueの間bodyを実行します".to_owned(),
            body: Box::new(|expr, step, global_context, local_context, args| {
                if args.len() != 3 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 3), expr.clone()));
                }
                let condgen = &args[0];
                let accgen = &args[1];
                let mut acc = args[2].clone();
                let mut step = step + 1;
                loop {
                    let (cond, next_step) = eval_apply(
                        expr,
                        step,
                        global_context,
                        local_context,
                        condgen.clone(),
                        vec![acc.clone()],
                    )?;
                    step = next_step;
                    if val_as_bool(&cond) != Some(true) {
                        break;
                    }
                    let (nextacc, next_step) = eval_apply(
                        expr,
                        step,
                        global_context,
                        local_context,
                        accgen.clone(),
                        vec![acc],
                    )?;
                    acc = nextacc;
                    step = next_step;
                }
                Ok((acc, step + 1))
            }),
        },
        EvalStdLibFun::Sort => LibFun {
            name: "sort".to_owned(),
            alias: vec![],
            usage: "`sort(list)`".to_owned(),
            note: "listをソートします".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                match args[0].clone() {
                    EvalResult::List(l) => {
                        let mut new_list = l;
                        new_list.sort_by(|a, b| match (val_as_float(a), val_as_float(b)) {
                            (Some(f1), Some(f2)) => f1.partial_cmp(&f2).unwrap(),
                            _ => std::cmp::Ordering::Equal,
                        });
                        Ok((EvalResult::List(new_list), step + 1))
                    }
                    _ => Err((EvalError::NotAList(args[0].clone()), expr.clone())),
                }
            }),
        },
        EvalStdLibFun::Sum => LibFun {
            name: "sum".to_owned(),
            alias: vec![],
            usage: "`sum(list)`".to_owned(),
            note: "listの要素の合計を返します".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                match val_as_list(&args[0]) {
                    Some(l) => {
                        let mut sum = 0.0;
                        for e in l {
                            match val_as_float(&e) {
                                Some(f) => sum += f,
                                _ => return Err((EvalError::NotANumber(e.clone()), expr.clone())),
                            }
                        }
                        Ok((EvalResult::FVal(sum), step + 1))
                    }
                    _ => Err((EvalError::NotAList(args[0].clone()), expr.clone())),
                }
            }),
        },
        EvalStdLibFun::Average => LibFun {
            name: "average".to_owned(),
            alias: vec!["ave".to_owned()],
            usage: "`average(list)`".to_owned(),
            note: "listの要素の平均を返します".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                match val_as_list(&args[0]) {
                    Some(l) => {
                        let mut sum = 0.0;
                        let mut count = 0;
                        for e in l {
                            match val_as_float(&e) {
                                Some(f) => {
                                    sum += f;
                                    count += 1;
                                }
                                _ => return Err((EvalError::NotANumber(e.clone()), expr.clone())),
                            }
                        }
                        if count == 0 {
                            return Err((EvalError::OutOfRange, expr.clone()));
                        }
                        Ok((EvalResult::FVal(sum / f64::from(count)), step + 1))
                    }
                    _ => Err((EvalError::NotAList(args[0].clone()), expr.clone())),
                }
            }),
        },
        EvalStdLibFun::Max => LibFun {
            name: "max".to_owned(),
            alias: vec![],
            usage: "`max(x, y, ...)`".to_owned(),
            note: "引数の最大値を返します".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.is_empty() {
                    return Err((EvalError::ArgCountMismatch(args.len(), 2), expr.clone()));
                }
                let mut max = f64::NEG_INFINITY;
                for e in args {
                    match val_as_float(&e) {
                        Some(f) => max = max.max(f),
                        _ => return Err((EvalError::NotANumber(e.clone()), expr.clone())),
                    }
                }
                Ok((EvalResult::FVal(max), step + 1))
            }),
        },
        EvalStdLibFun::Min => LibFun {
            name: "min".to_owned(),
            alias: vec![],
            usage: "`min(x, y, ...)`".to_owned(),
            note: "引数の最小値を返します".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.is_empty() {
                    return Err((EvalError::ArgCountMismatch(args.len(), 2), expr.clone()));
                }
                let mut min = f64::INFINITY;
                for e in args {
                    match val_as_float(&e) {
                        Some(f) => min = min.min(f),
                        _ => return Err((EvalError::NotANumber(e.clone()), expr.clone())),
                    }
                }
                Ok((EvalResult::FVal(min), step + 1))
            }),
        },
        EvalStdLibFun::Maximum => LibFun {
            name: "maximum".to_owned(),
            alias: vec![],
            usage: "`maximum(list)`".to_owned(),
            note: "listの最大値を返します".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                match val_as_list(&args[0]) {
                    Some(l) => {
                        let mut max = f64::NEG_INFINITY;
                        for e in l {
                            match val_as_float(&e) {
                                Some(f) => max = max.max(f),
                                _ => return Err((EvalError::NotANumber(e.clone()), expr.clone())),
                            }
                        }
                        Ok((EvalResult::FVal(max), step + 1))
                    }
                    _ => Err((EvalError::NotAList(args[0].clone()), expr.clone())),
                }
            }),
        },
        EvalStdLibFun::Minimum => LibFun {
            name: "minimum".to_owned(),
            alias: vec![],
            usage: "`minimum(list)`".to_owned(),
            note: "listの最小値を返します".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                match val_as_list(&args[0]) {
                    Some(l) => {
                        let mut min = f64::INFINITY;
                        for e in l {
                            match val_as_float(&e) {
                                Some(f) => min = min.min(f),
                                _ => return Err((EvalError::NotANumber(e.clone()), expr.clone())),
                            }
                        }
                        Ok((EvalResult::FVal(min), step + 1))
                    }
                    _ => Err((EvalError::NotAList(args[0].clone()), expr.clone())),
                }
            }),
        },
        EvalStdLibFun::Fix => LibFun {
            name: "fix".to_owned(),
            alias: vec![],
            usage: "`fix(f)`".to_owned(),
            note: "fの不動点を返します".to_owned(),
            body: Box::new(|expr, step, global_context, local_context, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                let func = args[0].clone();
                let xfyxxy = Expr::Lambda(
                    vec!["_x".to_owned()],
                    Box::new(Expr::Apply(
                        Box::new(Expr::Const("_f".to_owned())),
                        vec![Expr::Lambda(
                            vec!["_y".to_owned()],
                            Box::new(Expr::Apply(
                                Box::new(Expr::Apply(
                                    Box::new(Expr::Const("_x".to_owned())),
                                    vec![
                                        Expr::Const("_x".to_owned()),
                                        Expr::Const("_y".to_owned()),
                                    ],
                                )),
                                vec![Expr::Const("_y".to_owned())],
                            )),
                        )],
                    )),
                );
                let z = Expr::Lambda(
                    vec!["_f".to_owned()],
                    Box::new(Expr::Apply(Box::new(xfyxxy.clone()), vec![xfyxxy])),
                );
                let (zval, _) = eval_expr_ctx(&z, step + 1, false, global_context, local_context)?;
                eval_apply(
                    expr,
                    step + 1,
                    global_context,
                    local_context,
                    zval,
                    vec![func],
                )
            }),
        },
        EvalStdLibFun::Pick => LibFun {
            name: "pick".to_owned(),
            alias: vec![],
            usage: "`pick(list)`".to_owned(),
            note: "listからランダムに要素を選択します".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                match val_as_list(&args[0]) {
                    Some(l) => {
                        if l.is_empty() {
                            return Err((EvalError::OutOfRange, expr.clone()));
                        }
                        let i = rand::thread_rng().gen_range(0..l.len());
                        Ok((l[i].clone(), step + 1))
                    }
                    _ => Err((EvalError::NotAList(args[0].clone()), expr.clone())),
                }
            }),
        },
        EvalStdLibFun::PickArg => LibFun {
            name: "pickarg".to_owned(),
            alias: vec![],
            usage: "`pickarg(x, y, ...)`".to_owned(),
            note: "引数からランダムに要素を選択します".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.is_empty() {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                let i = rand::thread_rng().gen_range(0..args.len());
                Ok((args[i].clone(), step + 1))
            }),
        },
        EvalStdLibFun::Shuffle => LibFun {
            name: "shuffle".to_owned(),
            alias: vec![],
            usage: "`shuffle(list)`".to_owned(),
            note: "listをシャッフルします".to_owned(),
            body: Box::new(|expr, step, _, _, args| {
                if args.len() != 1 {
                    return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
                }
                val_as_list(&args[0]).map_or_else(
                    || Err((EvalError::NotAList(args[0].clone()), expr.clone())),
                    |mut l| {
                        let mut shuffled = Vec::new();
                        while !l.is_empty() {
                            let i = rand::thread_rng().gen_range(0..l.len());
                            shuffled.push(l.remove(i));
                        }
                        Ok((EvalResult::List(shuffled), step + 1))
                    },
                )
            }),
        },
        EvalStdLibFun::Help => LibFun {
            name: "help".to_owned(),
            alias: vec![],
            usage: "`help()` or `help(func)`".to_owned(),
            note: "利用可能な関数の一覧を表示します。標準ライブラリの関数を渡すと説明を表示します"
                .to_owned(),
            body: Box::new(|_, step, _, _, args| {
                if args.len() == 1 {
                    match args[0].clone() {
                        EvalResult::FuncStdLib(f) => {
                            let libfun = get_libfun(f);
                            Ok((EvalResult::SVal(help_libfun(&libfun)), step + 1))
                        }
                        _ => Ok((
                            EvalResult::SVal("関数の説明を表示するには標準ライブラリの関数を直接引数に入れてください。例：`help(foldl)`".to_owned()),
                            step + 1,
                        )),
                    }
                } else {
                    let mut help = String::new();
                    help.push_str("Available functions: ");
                    for stdlibfun in EvalStdLibFun::iter() {
                        help.push_str(&format!("{}, ", stdlibfun));
                    }
                    Ok((EvalResult::SVal(help), step + 1))
                }
            }),
        },
        //_ => panic!("function not implemented: {:?}", func),
    }
}

pub fn match_const(s: &str) -> Option<EvalResult> {
    match s {
        "pi" => Some(EvalResult::FVal(std::f64::consts::PI)),
        "e" => Some(EvalResult::FVal(std::f64::consts::E)),
        "true" => Some(EvalResult::BVal(true)),
        "false" => Some(EvalResult::BVal(false)),
        "if" => Some(EvalResult::FuncIf),
        "lazy" => Some(EvalResult::FuncLazy),
        _ => None,
    }
}

pub fn stdlib_list() -> Vec<(String, EvalStdLibFun)> {
    let mut libfuns = Vec::new();
    for func in EvalStdLibFun::iter() {
        let libfun = get_libfun(func.clone());
        libfuns.push((libfun.name.clone(), func.clone()));
        for alias in libfun.alias.iter() {
            libfuns.push((alias.clone(), func.clone()));
        }
    }
    libfuns
}

#[derive(Debug, Clone, PartialEq, Eq, EnumIter)]
pub enum EvalStdLibFun {
    //Print,
    //Println,
    Sin,
    Cos,
    Tan,
    LogE,
    Log10,
    Log2,
    Abs,
    Floor,
    Ceil,
    Round,
    URand,   // 0.0 <= x < 1.0 uniform random
    GRand,   // standerd gaussian random
    Map,     // map(f, list)
    Geni,    // geni(f, n) = [f(0), f(1), ..., f(n-1)]
    Repeat,  // repeat(f, n) = [f(), f(), ..., f()]
    Filter,  // filter(f, list)
    ZipWith, // zipWith(f, list1, list2)
    Foldl,   // foldl(f, init, list)
    Foldr,   // foldr(f, init, list)
    Range,   // range(end) or range(start, end) or range(start, end, step)
    Join,    // join(list, sep)
    Slice,   // slice(list, start, end)
    Len,     // len(list)
    Head,    // head(list)
    Tail,    // tail(list)
    Last,    // last(list)
    Init,    // init(list)
    While,   // while(acc => cond, acc => nextacc, init)
    Sort,    // sort(list)
    Sum,     // sum(list)
    Average, // average(list)
    Max,     // max(x, y, ...)
    Min,     // min(x, y, ...)
    Maximum, // maximum(list)
    Minimum, // minimum(list)
    Fix,     // Fix(f) = f(Fix(f))
    Help,    // help() = "sin, cos, ..."
    Pick,    // pick(list)
    PickArg, // pickarg(x, y, ...)
    Shuffle, // shuffle(list)
}

impl std::fmt::Display for EvalStdLibFun {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let libfun = get_libfun(self.clone());
        libfun.name.fmt(f)
    }
}

type LibFunBody = dyn Fn(
    &Expr,
    usize,
    &Context,
    &Context,
    Vec<EvalResult>,
) -> Result<(EvalResult, usize), (EvalError, Expr)>;

pub struct LibFun {
    name: String,
    alias: Vec<String>,
    usage: String,
    note: String,
    body: Box<LibFunBody>,
    /*
    fn(
        expr: &Expr,
        step: usize,
        global_context: &Context,
        local_context: &Context,
        func: EvalStdLibFun,
        args: Vec<EvalResult>,
    ) -> Result<(EvalResult, usize), (EvalError, Expr)>)
    */
}

impl std::fmt::Debug for LibFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl std::fmt::Display for LibFun {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

// show help
fn help_libfun(
    LibFun {
        alias, usage, note, ..
    }: &LibFun,
) -> String {
    let mut s = format!("### {}\n", usage);
    if !note.is_empty() {
        s.push_str(&format!("{}\n", note));
    }
    if !alias.is_empty() {
        s.push_str(&format!(
            "alias: {}\n",
            alias
                .iter()
                .map(|a| format!("`{}`", a))
                .collect::<Vec<String>>()
                .join(", ")
        ));
    }
    s
}

fn generate_context(global_context: &Context) -> Context {
    let new_context = global_context.clone();
    let stdlib = stdlib_list();
    stdlib.into_iter().for_each(|(name, func)| {
        if new_context.contains_key(&name) {
            return;
        }
        new_context.insert(name, EvalResult::FuncStdLib(func));
    });
    new_context
}

pub fn eval_expr(expr: &Expr, global_context: &Context) -> Result<EvalResult, (EvalError, Expr)> {
    let libfun_context = generate_context(global_context);
    match eval_expr_ctx(expr, 0, true, &libfun_context, &DashMap::new()) {
        Ok((result, _)) => Ok(result),
        Err((e, expr)) => Err((e, expr)),
    }
}

pub fn eval_from_str(input: &str, global_context: &Context) -> Result<EvalResult, String> {
    match parse_expr(input) {
        Ok((_, expr)) => match eval_expr(&expr, global_context) {
            Ok(result) => Ok(result),
            Err((e, expr)) => Err(error_str((e, expr))),
        },
        Err(e) => Err(format!("Parse error: {:?}", e)),
    }
}

#[cfg(test)]
mod tests_parse {
    use super::*;

    #[test]
    fn test_parse_int() {
        assert_eq!(parse_int("123"), Ok(("", Expr::IVal(123))));
    }

    #[test]
    fn test_parse_float() {
        assert_eq!(parse_float("123.456"), Ok(("", Expr::FVal(123.456))));
    }

    #[test]
    fn test_parse_object() {
        assert_eq!(
            parse_expr("{a: 1, b: 2}"),
            Ok((
                "",
                Expr::Object(
                    vec![
                        ("a".to_owned(), Box::new(Expr::IVal(1))),
                        ("b".to_owned(), Box::new(Expr::IVal(2))),
                    ]
                    .into_iter()
                    .collect(),
                ),
            )),
        );
    }

    #[test]
    fn test_parse_list() {
        assert_eq!(
            parse_expr("[1, 2, 3]"),
            Ok((
                "",
                Expr::List(vec![Expr::IVal(1), Expr::IVal(2), Expr::IVal(3)]),
            )),
        );
    }

    #[test]
    fn parse_object_get() {
        assert_eq!(
            parse_expr("{a: 1, b: 2}.a"),
            Ok((
                "",
                Expr::Get(
                    Box::new(Expr::Object(
                        vec![
                            ("a".to_owned(), Box::new(Expr::IVal(1))),
                            ("b".to_owned(), Box::new(Expr::IVal(2))),
                        ]
                        .into_iter()
                        .collect(),
                    )),
                    "a".to_owned(),
                ),
            )),
        );
    }

    #[test]
    fn test_parse_list_at() {
        assert_eq!(
            parse_expr("[1, 2, 3][1]"),
            Ok((
                "",
                Expr::At(
                    Box::new(Expr::List(vec![
                        Expr::IVal(1),
                        Expr::IVal(2),
                        Expr::IVal(3)
                    ])),
                    Box::new(Expr::IVal(1)),
                ),
            )),
        );
    }

    #[test]
    fn test_parse_named_const() {
        assert_eq!(
            parse_named_const("pi"),
            Ok(("", Expr::Const("pi".to_owned())))
        );
    }

    #[test]
    fn test_parse_apply() {
        assert_eq!(
            parse_expr("f(1, 2)"),
            Ok((
                "",
                Expr::Apply(
                    Box::new(Expr::Const("f".to_owned())),
                    vec![Expr::IVal(1), Expr::IVal(2)],
                ),
            )),
        );
    }

    #[test]
    fn test_parse_dice() {
        assert_eq!(
            parse_expr("3d6*10"),
            Ok((
                "",
                Expr::Op2(
                    ExprOp2::Mul,
                    Box::new(Expr::Op2(
                        ExprOp2::Dice,
                        Box::new(Expr::IVal(3)),
                        Box::new(Expr::IVal(6)),
                    )),
                    Box::new(Expr::IVal(10)),
                ),
            )),
        );
    }

    #[test]
    fn test_parse_longexpr() {
        match parse_expr("1*2+3/4 - 5 % 6 ^ 7 ^ 8 * 9 + 0") {
            Ok((_, expr)) => println!("{}", expr),
            Err(e) => println!("{:?}", e),
        }
    }

    #[test]
    fn test_parse_paren() {
        assert_eq!(
            parse_expr("(1 + 2) * 3"),
            Ok((
                "",
                Expr::Op2(
                    ExprOp2::Mul,
                    Box::new(Expr::Op2(
                        ExprOp2::Add,
                        Box::new(Expr::IVal(1)),
                        Box::new(Expr::IVal(2))
                    )),
                    Box::new(Expr::IVal(3))
                ),
            ),),
        );
    }

    #[test]
    fn test_parse_lambda() {
        assert_eq!(
            parse_expr("(x, y, z) => x + y + z"),
            Ok((
                "",
                Expr::Lambda(
                    vec!["x".to_owned(), "y".to_owned(), "z".to_owned()],
                    Box::new(Expr::Op2(
                        ExprOp2::Add,
                        Box::new(Expr::Op2(
                            ExprOp2::Add,
                            Box::new(Expr::Const("x".to_owned())),
                            Box::new(Expr::Const("y".to_owned()))
                        )),
                        Box::new(Expr::Const("z".to_owned())),
                    )),
                ),
            )),
        );
    }

    #[test]
    fn test_parse_string() {
        assert_eq!(
            parse_expr("\"Hello, \nworld!\""),
            Ok(("", Expr::SVal("Hello, \nworld!".to_owned()))),
        );
    }

    #[test]
    fn test_parse_long2() {
        println!("{:?}", parse_expr("sum(grand()<0.5)"));
    }

    #[test]
    fn test_parse_apply_parts() {
        println!("{:?}", parse_apply("(grand()<0.5)"));
    }
}

#[cfg(test)]
mod tests_eval {
    use super::*;

    #[test]
    fn test_eval_longexpr() {
        let expr = parse_expr("((f,x)=>f(f(x)))((x)=>x*6,100)").unwrap().1;
        let context = DashMap::new();
        assert_eq!(
            EvalResult::IVal(100 * 6 * 6),
            eval_expr(&expr, &context).unwrap()
        );
    }

    #[test]
    fn test_eval_dice() {
        let expr = parse_expr("10000d20").unwrap().1;
        let context = DashMap::new();
        match eval_expr(&expr, &context) {
            Ok(EvalResult::IVal(i)) => {
                println!("{}", i);
                assert!((50000..=150000).contains(&i));
            }
            _ => std::panic!(),
        }
    }

    #[test]
    fn test_eval_dice2() {
        let expr = parse_expr("1d2").unwrap().1;
        let context = DashMap::new();
        let result = eval_expr(&expr, &context);
        match result {
            Ok(EvalResult::IVal(i)) => {
                println!("{}", i);
            }
            _ => {
                println!("{:?}", result);
                std::panic!();
            }
        }
    }

    #[test]
    fn test_string() {
        let expr = parse_expr("\"Hello, \\nworld!\\u{1f305}\"").unwrap().1;
        let context = DashMap::new();
        match eval_expr(&expr, &context) {
            Ok(EvalResult::SVal(s)) => {
                println!("{}", s);
            }
            _ => {
                std::panic!();
            }
        }
    }

    #[test]
    fn test_eval_object() {
        let expr = parse_expr("{a: 1, b: {c: 100, d: 200}}.b").unwrap().1;
        let context = DashMap::new();
        match eval_expr(&expr, &context) {
            Ok(EvalResult::Object(o)) => {
                println!("{:?}", o);
            }
            _ => {
                std::panic!();
            }
        }
    }
}
