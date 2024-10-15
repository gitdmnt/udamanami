use nom::{
  character::complete::{char, digit1, multispace0, alpha1, alphanumeric1, one_of, none_of, anychar},
  bytes::complete::tag,
  combinator::{map, recognize, value},
  sequence::{delimited, separated_pair, pair, preceded, tuple},
  multi::{separated_list0, fold_many0, many0_count, many1_count, many0},
  branch::alt,
  IResult,
};
use core::panic;
use std::{collections::HashMap, f64::consts::E};
use rand::{prelude::Distribution, Rng};
use rand_distr::StandardNormal;
use dashmap::DashMap;


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
      ExprOp2::Add => write!(f, "+"),
      ExprOp2::Sub => write!(f, "-"),
      ExprOp2::Mul => write!(f, "*"),
      ExprOp2::Div => write!(f, "/"),
      ExprOp2::Mod => write!(f, "%"),
      ExprOp2::Pow => write!(f, "^"),
      ExprOp2::Dice => write!(f, "d"),
      ExprOp2::Gt => write!(f, ">"),
      ExprOp2::Ge => write!(f, ">="),
      ExprOp2::Lt => write!(f, "<"),
      ExprOp2::Le => write!(f, "<="),
      ExprOp2::Eq => write!(f, "=="),
      ExprOp2::Ne => write!(f, "!="),
      ExprOp2::AndL => write!(f, "&&"),
      ExprOp2::OrL => write!(f, "||"),
      ExprOp2::XorL => write!(f, "^^"),
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprOp1 {
  Neg,
  OneDice,
  NotL,
}

impl std::fmt::Display for ExprOp1 {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      ExprOp1::Neg => write!(f, "-"),
      ExprOp1::OneDice => write!(f, "d"),
      ExprOp1::NotL => write!(f, "!"),
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
  IVal(i64),
  FVal(f64),
  BVal(bool),
  SVal(String),
  List(Vec<Box<Expr>>),
  At(Box<Expr>, Box<Expr>),
  Object(HashMap<String, Box<Expr>>),
  Get(Box<Expr>, String),
  Const(String),
  Op1(ExprOp1, Box<Expr>),
  Op2(ExprOp2, Box<Expr>, Box<Expr>),
  Apply(Box<Expr>, Vec<Box<Expr>>),
  Lambda(Vec<String>, Box<Expr>),
}

impl std::fmt::Display for Expr {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Expr::IVal(i) => write!(f, "{}", i),
      Expr::FVal(v) => write!(f, "{}", v),
      Expr::BVal(b) => write!(f, "{}", b),
      Expr::SVal(s) => write!(f, "\"{}\"", s.escape_debug()),
      Expr::List(l) => write!(f, "[{}]", l.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(", ")),
      Expr::At(e1, e2) => write!(f, "{}[{}]", e1, e2),
      Expr::Object(o) => write!(f, "{{{}}}", o.iter().map(|(k, v)| format!("{}: {}", k, v.to_string())).collect::<Vec<String>>().join(", ")),
      Expr::Get(e, k) => write!(f, "{}.{}", e, k),
      Expr::Const(s) => write!(f, "{}", s),
      Expr::Op1(op, e) => write!(f, "{}{}", op, e),
      Expr::Op2(op, e1, e2) => write!(f, "({} {} {})", e1, op, e2),
      Expr::Apply(name, args) => {
        write!(f, "{}({})", name, args.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(", "))
      },
      Expr::Lambda(params, body) => {
        write!(f, "({} => {})", params.join(", "), body)
      },
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

fn parse_binop_left (op_parser: fn(&str) -> IResult<&str, ExprOp2>, next_parser: fn(&str) -> IResult<&str, Expr>) -> (impl Fn(&str) -> IResult<&str, Expr>) {
  move |input: &str| {
    let (input, init) = next_parser(input)?;
    fold_many0(
      pair(preceded(multispace0, op_parser), next_parser),
      move || init.clone(),
      |acc, (op, val)| Expr::Op2(op, Box::new(acc), Box::new(val)),
    )(input)
  }
}

fn parse_binop_right(op_parser: fn(&str) -> IResult<&str, ExprOp2>, next_parser: fn(&str) -> IResult<&str, Expr>) -> impl Fn(&str) -> IResult<&str, Expr> {
    move |input: &str| {
        let (input, init) = next_parser(input)?;
        let (input, (mut exprs, ops)) = fold_many0(
            pair(preceded(multispace0, op_parser), next_parser),
            || (vec![init.clone()],Vec::new()),
            |(mut exprs, mut ops), (op, val)| {
                exprs.push(val);
                ops.push(op);
                (exprs, ops)
            },
        )(input)?;

        let last = exprs.pop().unwrap();

        let result = ops.into_iter().rev().zip(exprs.into_iter().rev()).fold(last, |acc, (op, val)| Expr::Op2(op, Box::new(val), Box::new(acc)));

        Ok((input, result))
    }
}

//無結合 同順位の演算子の連続は受理しない
fn parse_binop_none(op_parser: fn(&str) -> IResult<&str, ExprOp2>, next_parser: fn(&str) -> IResult<&str, Expr>) -> impl Fn(&str) -> IResult<&str, Expr> {
    move |input: &str| {
      map(
        tuple((next_parser, preceded(multispace0, op_parser), next_parser)),
        |(e1, op, e2)| Expr::Op2(op, Box::new(e1), Box::new(e2)),
      )(input)
    }
}

fn parse_term_l(op_parser: fn(&str) -> IResult<&str, ExprOp2>, next_parser: fn(&str) -> IResult<&str, Expr>) -> impl Fn(&str) -> IResult<&str, Expr> {
    move |input: &str| {
      preceded(multispace0, alt((parse_binop_left(op_parser, next_parser), next_parser)))(input)
    }
}

fn parse_term_r(op_parser: fn(&str) -> IResult<&str, ExprOp2>, next_parser: fn(&str) -> IResult<&str, Expr>) -> impl Fn(&str) -> IResult<&str, Expr> {
    move |input: &str| {
      preceded(multispace0, alt((parse_binop_right(op_parser, next_parser), next_parser)))(input)
    }
}

fn parse_term_n(op_parser: fn(&str) -> IResult<&str, ExprOp2>, next_parser: fn(&str) -> IResult<&str, Expr>) -> impl Fn(&str) -> IResult<&str, Expr> {
    move |input: &str| {
      preceded(multispace0, alt((parse_binop_none(op_parser, next_parser), next_parser)))(input)
    }
}


// 1文字目は数字以外。アルファベット・アンダースコア・数字を許容
fn parse_identifier(input: &str) -> IResult<&str, &str> {
  preceded(multispace0,
  recognize(
    pair(
      alt((alpha1, tag("_"))),
      many0_count(alt((alphanumeric1, tag("_"))))
    )
  ))(input)
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
  map(
    parse_identifier,
    |s: &str| Expr::Const(s.to_string()),
  )(input)
}

// (x, y, z) => x + y + z
// to Lambda(["x", "y", "z"], [Op2(Add, Op2(Add, Var("x"), Var("y")), Var("z"))])
fn parse_lambda(input: &str) -> IResult<&str, Expr> {
  map(
    pair(
      delimited(preceded(multispace0, char('(')), separated_list0(char(','), preceded(multispace0, parse_identifier)), preceded(multispace0, char(')'))),
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
    |(arg, body)| Expr::Lambda(vec![arg.to_string()], Box::new(body)),
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
  preceded(
      char('\\'),
      anychar,
  )(input)
}

fn parse_string_content(input: &str) -> IResult<&str, String> {
  map(
      many0(
          alt((
              parse_special_escape,
              parse_escaped_char,
              none_of("\""),
          )),
      ),
      |chars| chars.into_iter().collect(),
  )(input)
}


fn parse_string_literal(input: &str) -> IResult<&str, Expr> {
  map(
    delimited(
    char('\"'),
    parse_string_content,
    char('\"'),
    ),
    |s| Expr::SVal(s),
  )(input)
}

fn parse_list_literal(input: &str) -> IResult<&str, Expr> {
  map(
    delimited(
      char('['),
      separated_list0(char(','), parse_expr),
      char(']'),
    ),
    |exprs| Expr::List(exprs.into_iter().map(Box::new).collect()),
  )(input)
}

fn parse_object_literal(input: &str) -> IResult<&str, Expr> {
  map(
    delimited(
      char('{'),
      separated_list0(char(','), separated_pair(parse_identifier, preceded(multispace0, char(':')), parse_expr)),
      char('}'),
    ),
    |pairs| Expr::Object(pairs.into_iter().map(|(k, v)| (k.to_string(), Box::new(v))).collect()),
  )(input)
}

fn parse_term_before_postfix(input: &str) -> IResult<&str, Expr> {
  preceded(multispace0, alt((
    parse_lambda,
    parse_lambda_one,
    parse_paren,
    parse_int,
    parse_float,
    parse_named_const,
    parse_string_literal,
    parse_list_literal,
    parse_object_literal,
  )))(input)
}

enum PostfixExprPart {
  PEPApply(Vec<Box<Expr>>),
  PEPListAt(Box<Expr>),
  PEPGet(String),
}

fn parse_apply(input: &str) -> IResult<&str, PostfixExprPart> {
  map(
      delimited(
        preceded(multispace0, char('(')),
        separated_list0(char(','), preceded(multispace0, parse_expr)),
        preceded(multispace0, char(')')),
      ),
    |args| PostfixExprPart::PEPApply(args.into_iter().map(Box::new).collect()),
  )(input)
}

fn parse_list_at(input: &str) -> IResult<&str, PostfixExprPart> {
  map(
    delimited(
      preceded(multispace0, char('[')),
      preceded(multispace0, parse_expr),
      preceded(multispace0, char(']')),
    ),
    |ix| PostfixExprPart::PEPListAt(Box::new(ix)),
  )(input)
}

fn parse_object_get(input: &str) -> IResult<&str, PostfixExprPart> {
  map(preceded(preceded(multispace0, char('.')), parse_identifier),
    |k| PostfixExprPart::PEPGet(k.to_string()),
  )(input)
}

fn postfix(expr: Expr, postfix: PostfixExprPart) -> Expr {
  match postfix {
    PostfixExprPart::PEPApply(args) => Expr::Apply(Box::new(expr), args.into_iter().collect()),
    PostfixExprPart::PEPListAt(ix) => Expr::At(Box::new(expr), ix),
    PostfixExprPart::PEPGet(k) => Expr::Get(Box::new(expr), k),
  }
}

// postfix(ApplyとListAt)を全部処理
// 方針：parse_term_before_postfixを取ったのち、parse_postfixを取れるだけ取って、左からたたみ込んで適用
fn parse_term0(input: &str) -> IResult<&str, Expr> {
  let (input, init) = parse_term_before_postfix(input)?;
  fold_many0(
    alt((parse_apply, parse_list_at, parse_object_get)),
    move || init.clone(),
    |acc, pep| postfix(acc, pep),
  )(input)
}

// 1: 単項演算子
fn parse_neg(input: &str) -> IResult<&str, Expr> {
  map(
    pair(char('-'), parse_term0),
    |(_, e)| Expr::Op1(ExprOp1::Neg, Box::new(e)),
  )(input)
}

fn parse_one_dice(input: &str) -> IResult<&str, Expr> {
  map(
    pair(one_of("dD"), parse_term0),
    |(_, e)| Expr::Op1(ExprOp1::OneDice, Box::new(e)),
  )(input)
}

fn parse_term1(input: &str) -> IResult<&str, Expr> {
  preceded(multispace0, alt((parse_neg, parse_one_dice, parse_term0)))(input)
}

// 2: D 左結合
fn parse_term2(input: &str) -> IResult<&str, Expr> {
    parse_term_l(
      |op| map(one_of("dD"), |_| ExprOp2::Dice)(op),
      parse_term1
    )(input)
}

// 3: ^ ** (←同義) 右結合
fn parse_term3(input: &str) -> IResult<&str, Expr> {
  parse_term_r(
    |op| map(alt((tag("^"), tag("**"))), |_| ExprOp2::Pow)(op),
    parse_term2
  )(input)
}

// 4: * / % 左結合
fn parse_term4(input: &str) -> IResult<&str, Expr> {
  parse_term_l(
    |op| alt((
      map(char('*'), |_| ExprOp2::Mul),
      map(char('/'), |_| ExprOp2::Div),
      map(char('%'), |_| ExprOp2::Mod),
    ))(op),
    parse_term3
  )(input)
}

// 5: + - 左結合
fn parse_term5(input: &str) -> IResult<&str, Expr> {
  parse_term_l(
    |op| alt((
      map(char('+'), |_| ExprOp2::Add),
      map(char('-'), |_| ExprOp2::Sub),
    ))(op),
    parse_term4
  )(input)
}

// 6: > >= < <= == != 無結合
fn parse_term6(input: &str) -> IResult<&str, Expr> {
  parse_term_n(
    |op| alt((
      map(tag(">="), |_| ExprOp2::Ge),
      map(tag("<="), |_| ExprOp2::Le),
      map(tag("=="), |_| ExprOp2::Eq),
      map(tag("!="), |_| ExprOp2::Ne),
      map(char('>'), |_| ExprOp2::Gt),
      map(char('<'), |_| ExprOp2::Lt),
    ))(op),
    parse_term5
  )(input)
}

// 7: && || ^^ 左結合
fn parse_term7(input: &str) -> IResult<&str, Expr> {
  parse_term_l(
    |op| alt((
      map(tag("&&"), |_| ExprOp2::AndL),
      map(tag("||"), |_| ExprOp2::OrL),
      map(tag("^^"), |_| ExprOp2::XorL),
    ))(op),
    parse_term6
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

#[derive(Debug, Clone, PartialEq)]
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
  URand,  // 0.0 <= x < 1.0 uniform random
  GRand,  // standerd gaussian random
  If,     // if(cond, then, else)
  Map,    // map(f, list)
  Filter, // filter(f, list)
  Foldl,  // foldl(f, init, list)
  Foldr,  // foldr(f, init, list)
  Range,  // range(end) or range(start, end) or range(start, end, step)
  Len,    // len(list)
  Head,   // head(list)
  Tail,   // tail(list)
  Init,   // init(list)
  Last,   // last(list)
  Fix,    // Fix(f) = f(Fix(f))
  While,  // while(acc => cond, acc => nextacc, init)
  Sort,   // sort(list)
  Sum,    // sum(list)
  Average,// average(list)
}

impl std::fmt::Display for EvalStdLibFun {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      EvalStdLibFun::Sin     => write!(f, "sin"),
      EvalStdLibFun::Cos     => write!(f, "cos"),
      EvalStdLibFun::Tan     => write!(f, "tan"),
      EvalStdLibFun::LogE    => write!(f, "ln"),
      EvalStdLibFun::Log10   => write!(f, "log10"),
      EvalStdLibFun::Log2    => write!(f, "log2"),
      EvalStdLibFun::Abs     => write!(f, "abs"),
      EvalStdLibFun::Floor   => write!(f, "floor"),
      EvalStdLibFun::Ceil    => write!(f, "ceil"),
      EvalStdLibFun::Round   => write!(f, "round"),
      EvalStdLibFun::URand   => write!(f, "urand"),
      EvalStdLibFun::GRand   => write!(f, "grand"),
      EvalStdLibFun::If      => write!(f, "if"),
      EvalStdLibFun::Map     => write!(f, "map"),
      EvalStdLibFun::Filter  => write!(f, "filter"),
      EvalStdLibFun::Foldl   => write!(f, "foldl"),
      EvalStdLibFun::Foldr   => write!(f, "foldr"),
      EvalStdLibFun::Range   => write!(f, "range"),
      EvalStdLibFun::Len     => write!(f, "len"),
      EvalStdLibFun::Head    => write!(f, "head"),
      EvalStdLibFun::Tail    => write!(f, "tail"),
      EvalStdLibFun::Init    => write!(f, "init"),
      EvalStdLibFun::Last    => write!(f, "last"),
      EvalStdLibFun::Fix     => write!(f, "fix"),
      EvalStdLibFun::While   => write!(f, "while"),
      EvalStdLibFun::Sort    => write!(f, "sort"),
      EvalStdLibFun::Sum     => write!(f, "sum"),
      EvalStdLibFun::Average => write!(f, "average"),
    }
  }
}



#[derive(Debug, Clone)]
pub enum EvalResult {
  IVal(i64),
  FVal(f64),
  BVal(bool),
  SVal(String),
  List(Vec<Box<EvalResult>>),
  Object(HashMap<String, Box<EvalResult>>),
  Closure(Vec<String>, Box<Expr>, Box<Context>),
  FuncStdLib(EvalStdLibFun),
  Lazy(Box<Expr>) //適用を受けるまで遅延
}

impl PartialEq for EvalResult {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (EvalResult::IVal(i1), EvalResult::IVal(i2)) => i1 == i2,
      (EvalResult::FVal(f1), EvalResult::FVal(f2)) => f1 == f2,
      (EvalResult::BVal(b1), EvalResult::BVal(b2)) => b1 == b2,
      (EvalResult::SVal(s1), EvalResult::SVal(s2)) => s1 == s2,
      (EvalResult::List(l1), EvalResult::List(l2)) => l1 == l2,
      (EvalResult::Object(o1), EvalResult::Object(o2)) => o1 == o2,
      (EvalResult::Closure(a, b, _), EvalResult::Closure(c, d, _)) => a == c && b == d,
      (EvalResult::FuncStdLib(f1), EvalResult::FuncStdLib(f2)) => f1 == f2,
      (EvalResult::Lazy(e1), EvalResult::Lazy(e2)) => e1 == e2,
      _ => false,
    }
  }
}

impl std::fmt::Display for EvalResult {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      EvalResult::IVal(i) => write!(f, "{}", i),
      EvalResult::FVal(v) => write!(f, "{}", v),
      EvalResult::BVal(b) => write!(f, "{}", b),
      EvalResult::SVal(s) => write!(f, "\"{}\"", s),
      EvalResult::List(l) => write!(f, "[{}]", l.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(", ")),
      EvalResult::Object(o) => write!(f, "{{{}}}", o.iter().map(|(k, v)| format!("{}: {}", k, v.to_string())).collect::<Vec<String>>().join(", ")),
      EvalResult::Closure(params, body, context) => write!(f, "({} => {})@{:?}", params.join(", "), body, context),
      EvalResult::FuncStdLib(fun) => write!(f, "{}", fun),
      EvalResult::Lazy(body) => write!(f, "Lazy({})", body),
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
      EvalError::NegativeDice => write!(f, "Negative dice"),
      EvalError::InvalidDice => write!(f, "Invalid dice"),
      EvalError::TooManyDice => write!(f, "Too many dice"),
      EvalError::UndefinedVar(s) => write!(f, "Undefined variable: {}", s),
      //EvalError::MismatchedType(t1, t2) => write!(f, "Mismatched type: {:?} and {:?}", t1, t2),
      EvalError::ArgCountMismatch(a, b) => write!(f, "Argument count mismatch: {} and {}", a, b),
      EvalError::StepLimitExceeded => write!(f, "Step limit exceeded"),
      EvalError::OutOfRange => write!(f, "Out of range"),
      EvalError::NotANumber(e) => write!(f, "{} is not a number", e),
      EvalError::NotAFunction(e) => write!(f, "{} is not a function", e),
      EvalError::NotAList(e) => write!(f, "{} is not a list", e),
      EvalError::NotAnIndex(e) => write!(f, "{} is not an index", e),
      EvalError::NotAnObject(e) => write!(f, "{} is not an object", e),
    }
  }
}

pub fn error_str((e, expr) : (EvalError, Expr)) -> String {
  format!("Error: {} at {}", e, expr)
}

fn val_as_float(val: &EvalResult) -> Option<f64> {
  match val {
    EvalResult::IVal(i) => Some(*i as f64),
    EvalResult::BVal(b) => Some(if *b {1.0} else {0.0}),
    EvalResult::FVal(f) => Some(*f),
    _ => None,
  }
}

fn val_as_int(val: &EvalResult) -> Option<i64> {
  match val {
    EvalResult::IVal(i)  => Some(*i),
    EvalResult::BVal(b) => Some(if *b {1} else {0}),
    EvalResult::FVal(f)  => Some(*f as i64),
    _ => None,
  }
}

fn val_as_precise_int(val: &EvalResult) -> Option<i64> {
  match val {
    EvalResult::IVal(i) => Some(*i),
    EvalResult::BVal(b) => Some(if *b {1} else {0}),
    _ => None,
  }
}

fn val_as_bool(val: &EvalResult) -> Option<bool> {
  match val {
    EvalResult::IVal(i) => Some(*i != 0),
    EvalResult::BVal(b) => Some(*b),
    EvalResult::FVal(f) => Some(*f != 0.0),
    _ => None,
  }
}

fn val_as_list(val: &EvalResult) -> Option<Vec<Box<EvalResult>>> {
  match val {
    EvalResult::List(l) => Some(l.clone()),
    EvalResult::SVal(s) => Some(s.chars().map(|c| Box::new(EvalResult::SVal(c.to_string()))).collect()),
    _ => None,
  }
}

fn val_numop2_if<F, G>(expr: &Expr, step: usize, val1: &EvalResult, val2: &EvalResult, intver: F, floatver: G) -> Result<(EvalResult, usize), (EvalError, Expr)>
where F: Fn(i64, i64) -> i64, G: Fn(f64, f64) -> f64 {
  match (val_as_precise_int(val1), val_as_precise_int(val2)) {
    (Some(i1), Some(i2))   => Ok((EvalResult::IVal(intver(i1, i2)), step + 1)),
    _ => match (val_as_float(val1), val_as_float(val2)) {
      (Some(f1), Some(f2)) => Ok((EvalResult::FVal(floatver(f1, f2)), step + 1)),
      (Some(_), _) => Err((EvalError::NotANumber(val2.clone()), expr.clone())),
      _=> Err((EvalError::NotANumber(val1.clone()), expr.clone())),
    },
  }
}

fn val_numop2_f<F>(expr: &Expr, step: usize, val1: &EvalResult, val2: &EvalResult, floatver: F) -> Result<(EvalResult, usize), (EvalError, Expr)>
where F: Fn(f64, f64) -> f64 {
  match (val_as_float(val1), val_as_float(val2)) {
    (Some(f1), Some(f2)) => Ok((EvalResult::FVal(floatver(f1, f2)), step + 1)),
    (Some(_), _) => Err((EvalError::NotANumber(val2.clone()), expr.clone())),
    _=> Err((EvalError::NotANumber(val1.clone()), expr.clone())),
  }
}

fn is_str(s: EvalResult) -> bool {
  match s {
    EvalResult::SVal(_) => true,
    _ => false,
  }
}

fn eval_expr_ctx(expr: &Expr, step: usize, global_context: &Context, local_context: &Context) -> Result<(EvalResult, usize), (EvalError, Expr)> {
  //println!("eval_expr_ctx: {:?} \u{1f31f} {:?}", expr, context);
  
  if step > 1000 {
    return Err((EvalError::StepLimitExceeded, expr.clone()));
  }
  match expr {
    Expr::IVal(i) => Ok((EvalResult::IVal(*i as i64), step)),
    Expr::FVal(f) => Ok((EvalResult::FVal(*f), step)),
    Expr::BVal(b) => Ok((EvalResult::BVal(*b), step)),
    Expr::SVal(s) => Ok((EvalResult::SVal(s.clone()), step)),
    Expr::List(l) => {
      let mut new_list = Vec::new();
      let mut steps = step + 1;
      for e in l {
        let (val, next_step) = eval_expr_ctx(e, steps, global_context, local_context)?;
        new_list.push(Box::new(val));
        steps = next_step;
      }
      Ok((EvalResult::List(new_list), steps))
    },
    Expr::Object(o) => {
      let mut new_obj = HashMap::new();
      let mut steps = step + 1;
      for (k, v) in o {
        let (val, next_step) = eval_expr_ctx(v, steps, global_context, local_context)?;
        new_obj.insert(k.clone(), Box::new(val));
        steps = next_step;
      }
      Ok((EvalResult::Object(new_obj), steps))
    },
    Expr::Get(e, k) => {
      let ( val, next_step) = eval_expr_ctx(e, step + 1, global_context, local_context)?;
      match val {
        EvalResult::Object(o) => match o.get(k) {
          Some(result) => Ok((*result.clone(), next_step + 1)),
          None => Err((EvalError::UndefinedVar(k.clone()), expr.clone())),
        },
        _ => Err((EvalError::NotAnObject(val) , expr.clone())),
      }
    },

    Expr::At(e1, e2) => {
      let (val1, next_step) = eval_expr_ctx(e1, step + 1, global_context, local_context)?;
      let (val2, next_step) = eval_expr_ctx(e2, next_step + 1, global_context, local_context)?;
      match val_as_list(&val1) {
        Some(v) => {
          let index = match val2 {
            EvalResult::IVal(i) => Ok(i as isize),
            EvalResult::FVal(f) => Ok(f as isize),
            _ => return Err((EvalError::NotAnIndex(val2), expr.clone())),
          }?;
          if index < 0 || index >= v.len() as isize{
            return Err((EvalError::OutOfRange, expr.clone()));
          }
          Ok((*v[index as usize].clone(), next_step + 1))
        },
        _ => Err((EvalError::NotAList(val1), expr.clone())),
      }
    },
    
    
    Expr::Const(s) => {
      match local_context.get(s) {
        Some(result) => Ok((result.clone(), step)),
        None => 
          match global_context.get(s) {
            Some(result) => Ok((result.clone(), step)),
            None => match match_const(s) {
              Some(result) => Ok((result, step)),
              None => Err((EvalError::UndefinedVar(s.clone()), expr.clone())),
            },
          }
      }
    },
    Expr::Op1(op, e) => {
      let (val, next_step) = eval_expr_ctx(e, step + 1, global_context, local_context)?;
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
          let r = rand::thread_rng().gen_range(1 ..= i);
          Ok((EvalResult::IVal(r), next_step + 1))
        },
        ExprOp1::NotL => Ok((EvalResult::BVal(fval == 0.0), next_step + 1)),
      }
    },
    Expr::Op2(op, e1, e2) => {
      let (val1, next_step) = eval_expr_ctx(e1, step + 1, global_context, local_context)?;
      let (val2, next_step) = eval_expr_ctx(e2, next_step + 1, global_context, local_context)?;

      match op {
        ExprOp2::Add => {
          if is_str(val1.clone()) || is_str(val2.clone()) {
            let str1 = match val1 {
              EvalResult::SVal(s) => s,
              _ => format!("{}", val1),
            };
            let str2 = match val2 {
              EvalResult::SVal(s) => s,
              _ => format!("{}", val2),
            };
            return Ok((EvalResult::SVal(format!("{}{}", str1, str2)), next_step + 1));
          }else{
            match (val1.clone(), val2.clone()) {
              (EvalResult::List(l1), EvalResult::List(l2)) => {
                let mut new_list = Vec::new();
                for e in l1 {
                  new_list.push(e.clone());
                }
                for e in l2 {
                  new_list.push(e.clone());
                }
                return Ok((EvalResult::List(new_list), next_step + 1));
              },
              _ => (),
            }
          }
        }
        _ => (),
      };
      
      let fval1 = match val_as_float(&val1) {
        Some(f) => f,
        None => return Err((EvalError::NotANumber(val1), expr.clone())),
      };

      let fval2 = match val_as_float(&val2) {
        Some(f) => f,
        None => return Err((EvalError::NotANumber(val2), expr.clone())),
      };

      let bval1 = if fval1 != 0.0 {true} else {false};
      let bval2 = if fval2 != 0.0 {true} else {false};
      
      match op {
        ExprOp2::Add => val_numop2_if(expr, step, &val1, &val2, |i1, i2| i1 + i2, |f1, f2| f1 + f2),
        ExprOp2::Sub => val_numop2_if(expr, step, &val1, &val2, |i1, i2| i1 - i2, |f1, f2| f1 - f2),
        ExprOp2::Mul => val_numop2_if(expr, step, &val1, &val2, |i1, i2| i1 * i2, |f1, f2| f1 * f2),
        ExprOp2::Mod => val_numop2_f (expr, step, &val1, &val2, |f1, f2| f1 % f2),
        ExprOp2::Pow => val_numop2_f (expr, step, &val1, &val2, |f1, f2| f1.powf(f2)),
        ExprOp2::Div => val_numop2_f (expr, step, &val1, &val2, |f1, f2| f1 / f2),
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
            sum += rand::thread_rng().gen_range(1 ..= size);
          }
          Ok((EvalResult::IVal(sum), next_step + 1))
        },
        ExprOp2::Gt => Ok((EvalResult::BVal(fval1 >  fval2), next_step + 1)),
        ExprOp2::Ge => Ok((EvalResult::BVal(fval1 >= fval2), next_step + 1)),
        ExprOp2::Lt => Ok((EvalResult::BVal(fval1 <  fval2), next_step + 1)),
        ExprOp2::Le => Ok((EvalResult::BVal(fval1 <= fval2), next_step + 1)),
        ExprOp2::Eq => Ok((EvalResult::BVal(fval1 == fval2), next_step + 1)),
        ExprOp2::Ne => Ok((EvalResult::BVal(fval1 != fval2), next_step + 1)),
        
        ExprOp2::AndL => Ok((EvalResult::BVal(bval1 && bval2), next_step + 1)),
        ExprOp2::OrL  => Ok((EvalResult::BVal(bval1 || bval2), next_step + 1)),
        ExprOp2::XorL => Ok((EvalResult::BVal(bval1 ^  bval2), next_step + 1)),
      }
    },
    Expr::Apply(fun, args) => {

      let (vfun, next_step) = eval_expr_ctx(fun, step + 1, global_context, local_context)?;

      match vfun {
        EvalResult::FuncStdLib(EvalStdLibFun::If) => {
          if args.len() != 3 {
            return Err((EvalError::ArgCountMismatch(args.len(), 3), expr.clone()));
          }
          let (cond, next_step) = eval_expr_ctx(&args[0], next_step, global_context, local_context)?;
          let bval = match val_as_bool(&cond) {
            Some(b) => b,
            None => return Err((EvalError::NotANumber(cond), expr.clone())),
          };
          let (val, next_step) = if bval {
            eval_expr_ctx(&args[1], next_step, global_context, local_context)?
          } else {
            eval_expr_ctx(&args[2], next_step, global_context, local_context)?
          };
          return Ok((val, next_step));
        },
        EvalResult::FuncStdLib(EvalStdLibFun::Fix) => {
          if args.len() != 1 {
            return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
          }
          //return Ok((EvalResult::FixPoint(args[0].clone()), next_step));
          //return Ok((EvalResult::FixPoint(args[0].clone()), next_step));
          let body = &args[0];
          let (bodyval, next_step) = eval_expr_ctx(&args[0], next_step, global_context, local_context)?;
          return eval_apply(&body, next_step, global_context, local_context, bodyval, vec![Box::new(EvalResult::Lazy(body.clone()))]);
        },
        EvalResult::Lazy(body) => {
          let newexpr = &Expr::Apply(body.clone(), args.clone());
          return eval_expr_ctx(newexpr, next_step, global_context, local_context);
        },
        _ => ()
      };


      let mut vargs = Vec::new();
      let mut steps = next_step;
      for e in args {
        let (val, next_step) = eval_expr_ctx(e, steps, global_context, local_context)?;
        vargs.push(Box::new(val));
        steps = next_step;
      }
      eval_apply(expr, steps, global_context, local_context, vfun, vargs)
    },
    Expr::Lambda(name, body) => Ok((EvalResult::Closure(name.clone(), body.clone(), Box::new(local_context.clone())), step)),
  }
}

pub fn eval_apply(expr: &Expr, steps: usize, global_context: &Context, local_context: &Context, func: EvalResult, args: Vec<Box<EvalResult>>) -> Result<(EvalResult, usize), (EvalError, Expr)> {
  
  match func {
    EvalResult::Closure(params, body, ctx) => {
      if args.len() != params.len() {
        return Err((EvalError::ArgCountMismatch(args.len(), params.len()), expr.clone()));
      }

      //contextをcloneしたのち、ctxにもあるものはctxのものを優先
      let new_context = local_context.clone();
      ctx.iter().for_each(|e| {
        new_context.insert(e.key().clone(), e.value().clone());
      });
      for (param, argval) in params.iter().zip(args.iter()) {
        new_context.insert(param.clone(), *argval.clone());
      }
      let steps = steps + 1;
      eval_expr_ctx(&body, steps, &global_context, &new_context)
    },
    EvalResult::FuncStdLib(libfun) => {
      let steps = steps + 1;
      eval_stdlib(expr, steps, global_context, local_context, libfun, args)
    },
    _ => Err((EvalError::NotAFunction(func), expr.clone())),
  }
}

pub fn eval_stdlib(expr: &Expr, step: usize, global_context: &Context, local_context: &Context, func: EvalStdLibFun, args: Vec<Box<EvalResult>>) -> Result<(EvalResult, usize), (EvalError, Expr)> {
  match func {
    EvalStdLibFun::Sin => {
      if args.len() != 1 {
        return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
      }
      match val_as_float(&args[0]) {
        Some(f) => Ok((EvalResult::FVal(f.sin()), step + 1)),
            _ => Err((EvalError::NotANumber(*args[0].clone()), expr.clone())),
          }
    },
    EvalStdLibFun::Cos => {
      if args.len() != 1 {
        return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
      }
      match val_as_float(&args[0]) {
        Some(f) => Ok((EvalResult::FVal(f.cos()), step + 1)),
        _ => Err((EvalError::NotANumber(*args[0].clone()), expr.clone())),
      }
    },
    EvalStdLibFun::Tan => {
      if args.len() != 1 {
        return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
      }
      match val_as_float(&args[0]) {
        Some(f) => Ok((EvalResult::FVal(f.tan()), step + 1)),
        _ => Err((EvalError::NotANumber(*args[0].clone()), expr.clone())),
      }
    },
    EvalStdLibFun::LogE => {
      if args.len() != 1 {
        return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
      }
      match val_as_float(&args[0]) {
        Some(f) => Ok((EvalResult::FVal(f.ln()), step + 1)),
        _ => Err((EvalError::NotANumber(*args[0].clone()), expr.clone())),
      }
    },
    EvalStdLibFun::Log10 => {
      if args.len() != 1 {
        return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
      }
      match val_as_float(&args[0]) {
        Some(f) => Ok((EvalResult::FVal(f.log10()), step + 1)),
        _ => Err((EvalError::NotANumber(*args[0].clone()), expr.clone())),
      }
    },
    EvalStdLibFun::Log2 => {
      if args.len() != 1 {
        return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
      }
      match val_as_float(&args[0]) {
        Some(f) => Ok((EvalResult::FVal(f.log2()), step + 1)),
        _ => Err((EvalError::NotANumber(*args[0].clone()), expr.clone())),
      }
    },
    EvalStdLibFun::Abs => {
      if args.len() != 1 {
        return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
      }

      match val_as_precise_int(&args[0]) {
        Some(i) => Ok((EvalResult::IVal(i.abs()), step + 1)),
        _ => 
        match val_as_float(&args[0]) {
          Some(f) => Ok((EvalResult::FVal(f.abs()), step + 1)),
          _ => Err((EvalError::NotANumber(*args[0].clone()), expr.clone())),
        }
      }
    },
    EvalStdLibFun::Floor => {
      if args.len() != 1 {
        return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
      }
      match val_as_float(&args[0]) {
        Some(f) => Ok((EvalResult::IVal(f.floor() as i64), step + 1)),
        _ => Err((EvalError::NotANumber(*args[0].clone()), expr.clone())),
      }
    },
    EvalStdLibFun::Ceil => {
      if args.len() != 1 {
        return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
      }
      match val_as_float(&args[0]) {
        Some(f) => Ok((EvalResult::IVal(f.ceil() as i64), step + 1)),
        _ => Err((EvalError::NotANumber(*args[0].clone()), expr.clone())),
      }
    },
    EvalStdLibFun::Round => {
      if args.len() != 1 {
        return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
      }
      match val_as_float(&args[0]) {
        Some(f) => Ok((EvalResult::IVal(f.round() as i64), step + 1)),
        _ => Err((EvalError::NotANumber(*args[0].clone()), expr.clone())),
      }
    },
    EvalStdLibFun::URand => {
      if args.len() != 0 {
        return Err((EvalError::ArgCountMismatch(args.len(), 0), expr.clone()));
      }
      Ok((EvalResult::FVal(rand::thread_rng().gen_range(0.0 .. 1.0)), step + 1))
    },
    EvalStdLibFun::GRand => {
      if args.len() != 0 {
        return Err((EvalError::ArgCountMismatch(args.len(), 0), expr.clone()));
      }
      Ok((EvalResult::FVal(rand::distributions::Standard.sample(&mut rand::thread_rng())), step + 1))
    },
    EvalStdLibFun::If => {
      //this should be handled in eval_expr_ctx
      //引数を正格評価すると死ぬので
      panic!("If should be handled in eval_expr_ctx");
    },
    EvalStdLibFun::Map => {
      if args.len() != 2 {
        return Err((EvalError::ArgCountMismatch(args.len(), 2), expr.clone()));
      }
      match val_as_list(&args[1]) {
        Some(l) => {
          let mut new_list = Vec::new();
          for e in l {
            let (val, next_step) = eval_apply(expr, step + 1, global_context, local_context, *args[0].clone(), vec![e.clone()])?;
            new_list.push(Box::new(val));
          }
          Ok((EvalResult::List(new_list), step + 1))
        },
        _ => Err((EvalError::NotAList(*args[1].clone()), expr.clone())),
      }
    },
    EvalStdLibFun::Filter => {
      if args.len() != 2 {
        return Err((EvalError::ArgCountMismatch(args.len(), 2), expr.clone()));
      }
      match val_as_list(&args[1]) {
        Some(l) => {
          let mut new_list = Vec::new();
          for e in l {
            let (val, next_step) = eval_apply(expr, step + 1, global_context, local_context, *args[0].clone(), vec![e.clone()])?;
            if val_as_bool(&val) == Some(true) {
              new_list.push(e.clone());
            }
          }
          Ok((EvalResult::List(new_list), step + 1))
        },
        _ => Err((EvalError::NotAList(*args[1].clone()), expr.clone())),
      }
    },
    EvalStdLibFun::Foldl => {
      if args.len() != 3 {
        return Err((EvalError::ArgCountMismatch(args.len(), 3), expr.clone()));
      }
      match val_as_list(&args[2]) {
        Some(l) => {
          let mut acc = *args[1].clone();
          let mut step = step + 1;
          for e in l {
            let (val, next_step) = eval_apply(expr, step, global_context, local_context, *args[0].clone(), vec![Box::new(acc), e.clone()])?;
            acc = val;
            step = next_step;
          }
          Ok((acc, step + 1))
        },
        _ => Err((EvalError::NotAList(*args[2].clone()), expr.clone())),
      }
    },
    EvalStdLibFun::Foldr => {
      if args.len() != 3 {
        return Err((EvalError::ArgCountMismatch(args.len(), 3), expr.clone()));
      }
      match val_as_list(&args[2]) {
        Some(l) => {
          let mut acc = *args[1].clone();
          let mut step = step + 1;
          for e in l.iter().rev() {
            let (val, next_step) = eval_apply(expr, step, global_context, local_context, *args[0].clone(), vec![e.clone(), Box::new(acc)])?;
            acc = val;
            step = next_step;
          }
          Ok((acc, step + 1))
        },
        _ => Err((EvalError::NotAList(*args[2].clone()), expr.clone())),
      }
    },
    EvalStdLibFun::Range => {
      let (start, stop, stepsize): (i64, i64, i64) =
        match args.len() {
          1 => 
            match val_as_int(&args[0]) {
              Some(e) => (0, e, 1),
              _ => return Err((EvalError::NotANumber(*args[0].clone()), expr.clone())),
            },
          2 =>
            match (val_as_int(&args[0]), val_as_int(&args[1])) {
              (Some(s), Some(e)) => (s, e, 1),
              (Some(_), _) => return Err((EvalError::NotANumber(*args[1].clone()), expr.clone())),
              _            => return Err((EvalError::NotANumber(*args[0].clone()), expr.clone())),
            },
          3 =>
            match (val_as_int(&args[0]), val_as_int(&args[1]), val_as_int(&args[2])) {
              (Some(s), Some(e), Some(p)) => (s, e, p),
              (Some(_), Some(_), _) => return Err((EvalError::NotANumber(*args[2].clone()), expr.clone())),
              (Some(_), _, _)       => return Err((EvalError::NotANumber(*args[1].clone()), expr.clone())),
              _                     => return Err((EvalError::NotANumber(*args[0].clone()), expr.clone())),
            },
          _ => return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone())),
        };
      
      let mut new_list = Vec::new();
      if step > 0 {
        for i in (start..stop).step_by(stepsize as usize) {
          new_list.push(Box::new(EvalResult::IVal(i)));
        }
      } else {
        for i in (start..stop).rev().step_by(stepsize.abs() as usize) {
          new_list.push(Box::new(EvalResult::IVal(i)));
        }
      }
      Ok((EvalResult::List(new_list), step + 1))
    },
    EvalStdLibFun::Len => {
      if args.len() != 1 {
        return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
      }
      match val_as_list(&args[0]) {
        Some(l) => Ok((EvalResult::IVal(l.len() as i64), step + 1)),
        _ => Err((EvalError::NotAList(*args[0].clone()), expr.clone())),
      }
    },
    EvalStdLibFun::Head => {
      if args.len() != 1 {
        return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
      }
      match val_as_list(&args[0]) {
        Some(l) => 
          if l.len() > 0 {
            Ok((*l[0].clone(), step + 1))
          } else {
            Err((EvalError::OutOfRange, expr.clone()))
          },
        _ => Err((EvalError::NotAList(*args[0].clone()), expr.clone())),
      }
    },
    EvalStdLibFun::Tail => {
      if args.len() != 1 {
        return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
      }
      match val_as_list(&args[0]) {
        Some(l) => 
          if l.len() > 0 {
            let mut new_list = Vec::new();
            for e in l.iter().skip(1) {
              new_list.push(e.clone());
            }
            Ok((EvalResult::List(new_list), step + 1))
          } else {
            Err((EvalError::OutOfRange, expr.clone()))
          },
        _ => Err((EvalError::NotAList(*args[0].clone()), expr.clone())),
      }
    },
    EvalStdLibFun::Last => {
      if args.len() != 1 {
        return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
      }
      match val_as_list(&args[0]) {
        Some(l) => 
          if l.len() > 0 {
            Ok((*l.last().unwrap().clone(), step + 1))
          } else {
            Err((EvalError::OutOfRange, expr.clone()))
          },
        _ => Err((EvalError::NotAList(*args[0].clone()), expr.clone())),
      }
    },
    EvalStdLibFun::Init => {
      if args.len() != 1 {
        return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
      }
      match val_as_list(&args[0]) {
        Some(l) => 
          if l.len() > 0 {
            let mut new_list = Vec::new();
            for e in l.iter().take(l.len() - 1) {
              new_list.push(e.clone());
            }
            Ok((EvalResult::List(new_list), step + 1))
          } else {
            Err((EvalError::OutOfRange, expr.clone()))
          },
        _ => Err((EvalError::NotAList(*args[0].clone()), expr.clone())),
      }
    },
    EvalStdLibFun::Fix => {
      // this should be handled in eval_expr
      panic!("Fix should not be called directly");
    },
    EvalStdLibFun::While => {
      if args.len() != 3 {
        return Err((EvalError::ArgCountMismatch(args.len(), 3), expr.clone()));
      }
      let condgen = &args[0];
      let accgen  = &args[1];
      let mut acc  = *args[2].clone();
      let mut step = step + 1;
      loop {
        let (cond, next_step) = eval_apply(expr, step, global_context, local_context, *condgen.clone(), vec![Box::new(acc.clone())])?;
        step = next_step;
        if val_as_bool(&cond) != Some(true) {
          break;
        }
        let (nextacc, next_step) = eval_apply(expr, step, global_context, local_context, *accgen.clone(), vec![Box::new(acc)])?;
        acc = nextacc;
        step = next_step;
      }
      Ok((acc, step + 1))
    },
    EvalStdLibFun::Sort => {
      if args.len() != 1 {
        return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
      }
      match *args[0].clone() {
        EvalResult::List(l) => {
          let mut new_list = l.clone();
          new_list.sort_by(|a, b| {
            match (val_as_float(a), val_as_float(b)) {
              (Some(f1), Some(f2)) => f1.partial_cmp(&f2).unwrap(),
              _ => std::cmp::Ordering::Equal,
            }
          });
          Ok((EvalResult::List(new_list), step + 1))
        },
        _ => Err((EvalError::NotAList(*args[0].clone()), expr.clone())),
          
      }
    },
    EvalStdLibFun::Sum => {
      if args.len() != 1 {
        return Err((EvalError::ArgCountMismatch(args.len(), 1), expr.clone()));
      }
      match val_as_list(&args[0]) {
        Some(l) => {
          let mut sum = 0.0;
          for e in l {
            match val_as_float(&e) {
              Some(f) => sum += f,
              _ => return Err((EvalError::NotANumber(*e.clone()), expr.clone())),
            }
          }
          Ok((EvalResult::FVal(sum), step + 1))
        },
        _ => Err((EvalError::NotAList(*args[0].clone()), expr.clone())),
      }
    },
    EvalStdLibFun::Average => {
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
              },
              _ => return Err((EvalError::NotANumber(*e.clone()), expr.clone())),
            }
          }
          if count == 0 {
            return Err((EvalError::OutOfRange, expr.clone()));
          }
          Ok((EvalResult::FVal(sum / count as f64), step + 1))
        },
        _ => Err((EvalError::NotAList(*args[0].clone()), expr.clone())),
      }
    },
  }
}

pub fn match_const(s: &str) -> Option<EvalResult> {
  match s {
    "pi" => Some(EvalResult::FVal(std::f64::consts::PI)),
    "e"  => Some(EvalResult::FVal(std::f64::consts::E)),
    "true" => Some(EvalResult::BVal(true)),
    "false" => Some(EvalResult::BVal(false)),

    // stdlib functions
    "sin"    => Some(EvalResult::FuncStdLib(EvalStdLibFun::Sin)),
    "cos"    => Some(EvalResult::FuncStdLib(EvalStdLibFun::Cos)),
    "tan"    => Some(EvalResult::FuncStdLib(EvalStdLibFun::Tan)),
    "loge"   => Some(EvalResult::FuncStdLib(EvalStdLibFun::LogE)),
    "logE"   => Some(EvalResult::FuncStdLib(EvalStdLibFun::LogE)),
    "ln"     => Some(EvalResult::FuncStdLib(EvalStdLibFun::LogE)),
    "log10"  => Some(EvalResult::FuncStdLib(EvalStdLibFun::Log10)),
    "log"    => Some(EvalResult::FuncStdLib(EvalStdLibFun::Log10)),
    "log2"   => Some(EvalResult::FuncStdLib(EvalStdLibFun::Log2)),
    "lg"     => Some(EvalResult::FuncStdLib(EvalStdLibFun::Log2)),
    "lb"     => Some(EvalResult::FuncStdLib(EvalStdLibFun::Log2)),
    "abs"    => Some(EvalResult::FuncStdLib(EvalStdLibFun::Abs)),
    "floor"  => Some(EvalResult::FuncStdLib(EvalStdLibFun::Floor)),
    "ceil"   => Some(EvalResult::FuncStdLib(EvalStdLibFun::Ceil)),
    "round"  => Some(EvalResult::FuncStdLib(EvalStdLibFun::Round)),
    "urand"  => Some(EvalResult::FuncStdLib(EvalStdLibFun::URand)),
    "grand"  => Some(EvalResult::FuncStdLib(EvalStdLibFun::GRand)),
    "if"     => Some(EvalResult::FuncStdLib(EvalStdLibFun::If)),
    "map"    => Some(EvalResult::FuncStdLib(EvalStdLibFun::Map)),
    "filter" => Some(EvalResult::FuncStdLib(EvalStdLibFun::Filter)),
    "foldl"  => Some(EvalResult::FuncStdLib(EvalStdLibFun::Foldl)),
    "foldr"  => Some(EvalResult::FuncStdLib(EvalStdLibFun::Foldr)),
    "range"  => Some(EvalResult::FuncStdLib(EvalStdLibFun::Range)),
    "len"    => Some(EvalResult::FuncStdLib(EvalStdLibFun::Len)),
    "head"   => Some(EvalResult::FuncStdLib(EvalStdLibFun::Head)),
    "tail"   => Some(EvalResult::FuncStdLib(EvalStdLibFun::Tail)),
    "last"   => Some(EvalResult::FuncStdLib(EvalStdLibFun::Last)),
    "init"   => Some(EvalResult::FuncStdLib(EvalStdLibFun::Init)),
    "fix"    => Some(EvalResult::FuncStdLib(EvalStdLibFun::Fix)),
    "while"  => Some(EvalResult::FuncStdLib(EvalStdLibFun::While)),
    "sort"   => Some(EvalResult::FuncStdLib(EvalStdLibFun::Sort)),
    "sum"    => Some(EvalResult::FuncStdLib(EvalStdLibFun::Sum)),
    "average" => Some(EvalResult::FuncStdLib(EvalStdLibFun::Average)),


    _ => None,
  }
}

pub fn eval_expr(expr: &Expr, global_context: &Context) -> Result<EvalResult, (EvalError, Expr)> {
  match eval_expr_ctx(expr, 0, global_context, &DashMap::new()) {
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
            ("a".to_string(), Box::new(Expr::IVal(1))),
            ("b".to_string(), Box::new(Expr::IVal(2))),
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
        Expr::List(vec![Box::new(Expr::IVal(1)), Box::new(Expr::IVal(2)), Box::new(Expr::IVal(3))]),
      )),
    );
  }

  # [test]
  fn parse_object_get() {
    assert_eq!(
      parse_expr("{a: 1, b: 2}.a"),
      Ok((
        "",
        Expr::Get(
          Box::new(Expr::Object(
            vec![
              ("a".to_string(), Box::new(Expr::IVal(1))),
              ("b".to_string(), Box::new(Expr::IVal(2))),
            ]
            .into_iter()
            .collect(),
          )),
          "a".to_string(),
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
          Box::new(Expr::List(vec![Box::new(Expr::IVal(1)), Box::new(Expr::IVal(2)), Box::new(Expr::IVal(3))])),
          Box::new(Expr::IVal(1)),
        ),
      )),
    );
  }


  #[test]
  fn test_parse_named_const() {
    assert_eq!(parse_named_const("pi"), Ok(("", Expr::Const("pi".to_string()))));
  }

  #[test]
  fn test_parse_apply() {
    assert_eq!(
      parse_expr("f(1, 2)"),
      Ok((
        "",
        Expr::Apply(
          Box::new(Expr::Const("f".to_string())),
          vec![Box::new(Expr::IVal(1)), Box::new(Expr::IVal(2))],
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
          Box::new(Expr::Op2(ExprOp2::Add, Box::new(Expr::IVal(1)), Box::new(Expr::IVal(2)))),
          Box::new(Expr::IVal(3))),
        ),
      ),
    );
  }

  #[test]
  fn test_parse_lambda() {
    assert_eq!(
      parse_expr("(x, y, z) => x + y + z"),
      Ok((
        "",
        Expr::Lambda(
          vec!["x".to_string(), "y".to_string(), "z".to_string()],
          Box::new(Expr::Op2(
            ExprOp2::Add,
            Box::new(Expr::Op2(ExprOp2::Add, Box::new(Expr::Const("x".to_string())), Box::new(Expr::Const("y".to_string())))),
            Box::new(Expr::Const("z".to_string())),
          )),
        ),
      )),
    );
  }
  

  #[test]
  fn test_parse_string() {
    assert_eq!(
      parse_expr("\"Hello, \nworld!\""),
      Ok(("", Expr::SVal("Hello, \nworld!".to_string()))),
    );
  }

}

#[cfg(test)]
mod tests_eval {
  use std::result;

use super::*;

  #[test]
  fn test_eval_longexpr() {
    let expr = parse_expr("((f,x)=>f(f(x)))((x)=>x*6,100)").unwrap().1;
    let context = DashMap::new();
    assert_eq!(EvalResult::IVal(100*6*6), eval_expr(&expr, &context).unwrap());
  }

  #[test]
  fn test_eval_dice() {
    let expr = parse_expr("10000d20").unwrap().1;
    let context = DashMap::new();
    match eval_expr(&expr, &context) {
      Ok(EvalResult::IVal(i)) => {
        println!("{}", i);
        assert!(i >= 50000 && i <= 150000);
      },
      _ => assert!(false),
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
      },
      _ => {
        println!("{:?}", result);
        assert!(false);
      },
    }
  }

  #[test]
  fn test_string() {
    let expr = parse_expr("\"Hello, \\nworld!\\u{1f305}\"").unwrap().1;
    let context = DashMap::new();
    match eval_expr(&expr, &context) {
      Ok(EvalResult::SVal(s)) => {
        println!("{}", s);
      },
      _ => {
        assert!(false);
      },
    }
  }

  #[test]
  fn test_eval_object() {
    let expr = parse_expr("{a: 1, b: {c: 100, d: 200}}.b").unwrap().1;
    let context = DashMap::new();
    match eval_expr(&expr,  &context) {
      Ok(EvalResult::Object(o)) => {
        println!("{:?}", o);
      },
      _ => {
        assert!(false);
      },
    }
  }
}