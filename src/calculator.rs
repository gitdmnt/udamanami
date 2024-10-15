use nom::{
  character::complete::{char, digit1, multispace0, alpha1, alphanumeric1, one_of, none_of, anychar},
  bytes::complete::tag,
  combinator::{map, recognize, value},
  sequence::{delimited, separated_pair, pair, preceded, tuple},
  multi::{separated_list0, fold_many0, many0_count, many1_count, many0},
  branch::alt,
  IResult,
};
use std::{collections::HashMap, f64::consts::E};
use rand::Rng;

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
      Expr::SVal(s) => write!(f, "{:?}", s.escape_debug()),
      Expr::List(l) => write!(f, "[{}]", l.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(", ")),
      Expr::At(e1, e2) => write!(f, "{}[{}]", e1, e2),
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
  recognize(
    pair(
      alt((alpha1, tag("_"))),
      many0_count(alt((alphanumeric1, tag("_"))))
    )
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
  )))(input)
}

enum PostfixExprPart {
  PEPApply(Vec<Box<Expr>>),
  PEPListAt(Box<Expr>),
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

fn postfix(expr: Expr, postfix: PostfixExprPart) -> Expr {
  match postfix {
    PostfixExprPart::PEPApply(args) => Expr::Apply(Box::new(expr), args.into_iter().collect()),
    PostfixExprPart::PEPListAt(ix) => Expr::At(Box::new(expr), ix),
  }
}

// postfix(ApplyとListAt)を全部処理
// 方針：parse_term_before_postfixを取ったのち、parse_postfixを取れるだけ取って、左からたたみ込んで適用
fn parse_term0(input: &str) -> IResult<&str, Expr> {
  let (input, init) = parse_term_before_postfix(input)?;
  fold_many0(
    alt((parse_apply, parse_list_at)),
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




#[derive(Debug, Clone, PartialEq)]
pub enum EvalResult {
  IVal(i64),
  FVal(f64),
  BVal(bool),
  SVal(String),
  List(Vec<Box<EvalResult>>),
  Lambda(Vec<String>, Box<Expr>),
}

impl std::fmt::Display for EvalResult {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      EvalResult::IVal(i) => write!(f, "{}", i),
      EvalResult::FVal(v) => write!(f, "{}", v),
      EvalResult::BVal(b) => write!(f, "{}", b),
      EvalResult::SVal(s) => write!(f, "\"{}\"", s),
      EvalResult::List(l) => write!(f, "[{}]", l.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(", ")),
      EvalResult::Lambda(params, body) => write!(f, "({} => {})", params.join(", "), body),
    }
  }
}


#[derive(Debug, Clone, PartialEq)]
pub enum EvalError {
  NotANumber,
  NotAFunction,
  NegativeDice,
  InvalidDice,
  TooManyDice,
  UndefinedVar(String),
  //MismatchedType(ResultType, ResultType),
  ArgCountMismatch(usize, usize),
  StepLimitExceeded,
  NotAList,
  NotAnIndex,
  OutOfRange,
}

impl std::fmt::Display for EvalError {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      EvalError::NotANumber => write!(f, "Not a number"),
      EvalError::NotAFunction => write!(f, "Not a function"),
      EvalError::NegativeDice => write!(f, "Negative dice"),
      EvalError::InvalidDice => write!(f, "Invalid dice"),
      EvalError::TooManyDice => write!(f, "Too many dice"),
      EvalError::UndefinedVar(s) => write!(f, "Undefined variable: {}", s),
      //EvalError::MismatchedType(t1, t2) => write!(f, "Mismatched type: {:?} and {:?}", t1, t2),
      EvalError::ArgCountMismatch(a, b) => write!(f, "Argument count mismatch: {} and {}", a, b),
      EvalError::StepLimitExceeded => write!(f, "Step limit exceeded"),
      EvalError::NotAList => write!(f, "Not a list"),
      EvalError::NotAnIndex => write!(f, "Not an index"),
      EvalError::OutOfRange => write!(f, "Out of range"),
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

fn val_numop2_if<F, G>(expr: &Expr, step: usize, val1: &EvalResult, val2: &EvalResult, intver: F, floatver: G) -> Result<(EvalResult, usize), (EvalError, Expr)>
where F: Fn(i64, i64) -> i64, G: Fn(f64, f64) -> f64 {
  match (val_as_precise_int(val1), val_as_precise_int(val2)) {
    (Some(i1), Some(i2))   => Ok((EvalResult::IVal(intver(i1, i2)), step + 1)),
    _ => match (val_as_float(val1), val_as_float(val2)) {
      (Some(f1), Some(f2)) => Ok((EvalResult::FVal(floatver(f1, f2)), step + 1)),
      _ => Err((EvalError::NotANumber, expr.clone())),
    },
  }
}

fn val_numop2_f<F>(expr: &Expr, step: usize, val1: &EvalResult, val2: &EvalResult, floatver: F) -> Result<(EvalResult, usize), (EvalError, Expr)>
where F: Fn(f64, f64) -> f64 {
  match (val_as_float(val1), val_as_float(val2)) {
    (Some(f1), Some(f2)) => Ok((EvalResult::FVal(floatver(f1, f2)), step + 1)),
    _ => Err((EvalError::NotANumber, expr.clone())),
  }
}

fn eval_expr_ctx(expr: &Expr, step: usize, context: &HashMap<String, EvalResult>) -> Result<(EvalResult, usize), (EvalError, Expr)> {
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
        let (val, next_step) = eval_expr_ctx(e, steps, context)?;
        new_list.push(Box::new(val));
        steps = next_step;
      }
      Ok((EvalResult::List(new_list), steps))
    },

    Expr::At(e1, e2) => {
      let (val1, next_step) = eval_expr_ctx(e1, step + 1, context)?;
      let (val2, next_step) = eval_expr_ctx(e2, next_step + 1, context)?;
      match val1 {
        (EvalResult::List(l)) => {
          let index = match val2 {
            EvalResult::IVal(i) => Ok(i as isize),
            EvalResult::FVal(f) => Ok(f as isize),
            _ => return Err((EvalError::NotAnIndex, expr.clone())),
          }?;
          if index < 0 || index >= l.len() as isize{
            return Err((EvalError::OutOfRange, expr.clone()));
          }
          Ok((*l[index as usize].clone(), next_step + 1))
        },
        (EvalResult::SVal(str)) => {
          let index = match val2 {
            EvalResult::IVal(i) => Ok(i as isize),
            EvalResult::FVal(f) => Ok(f as isize),
            _ => return Err((EvalError::NotAnIndex, expr.clone())),
          }?;
          if index < 0 || index >= str.chars().count() as isize{
            return Err((EvalError::OutOfRange, expr.clone()));
          }
          match str.chars().nth(index as usize) {
            Some(c) => Ok((EvalResult::SVal(c.to_string()), next_step + 1)),
            None => Err((EvalError::OutOfRange, expr.clone())),
          }
        },
        _ => Err((EvalError::NotAList, expr.clone())),
      }
    },
    
    
    Expr::Const(s) => {
      match context.get(s) {
        Some(result) => Ok((result.clone(), step)),
        None => 
          match s.as_str() {
            "pi" => Ok((EvalResult::FVal(std::f64::consts::PI), step)),
            "e"  => Ok((EvalResult::FVal(std::f64::consts::E) , step)),
            _ => Err((EvalError::UndefinedVar(s.clone()), expr.clone())),
          }
      }
    },
    Expr::Op1(op, e) => {
      let (val, next_step) = eval_expr_ctx(e, step + 1, context)?;
      let fval = match val {
        EvalResult::IVal(i) => i as f64,
        EvalResult::FVal(f) => f,
        _ => return Err((EvalError::NotANumber, expr.clone())),
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
      let (val1, next_step) = eval_expr_ctx(e1, step + 1, context)?;
      let (val2, next_step) = eval_expr_ctx(e2, next_step + 1, context)?;
      
      let fval1 = match val_as_float(&val1) {
        Some(f) => f,
        None => return Err((EvalError::NotANumber, expr.clone())),
      };

      let fval2 = match val_as_float(&val2) {
        Some(f) => f,
        None => return Err((EvalError::NotANumber, expr.clone())),
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
      let (vfun, next_step) = eval_expr_ctx(fun, step + 1, context)?;
      match vfun {
        EvalResult::Lambda(params, body) => {
          if args.len() != params.len() {
            return Err((EvalError::ArgCountMismatch(args.len(), params.len()), expr.clone()));
          }
          let mut new_context = context.clone();
          let mut steps = next_step + 1;
          for (param, arg) in params.iter().zip(args.iter()) {
            let (val, next_step) = eval_expr_ctx(arg, steps, context)?;
            new_context.insert(param.clone(), val);
            steps = next_step;
          }
          eval_expr_ctx(&body, steps, &new_context)
        },
        _ => Err((EvalError::NotAFunction, expr.clone())),
      }
    },
    Expr::Lambda(name, body) => Ok((EvalResult::Lambda(name.clone(), body.clone()), step)),
  }
}

pub fn eval_expr(expr: &Expr) -> Result<EvalResult, (EvalError, Expr)> {
  match eval_expr_ctx(expr, 0, &HashMap::new()) {
    Ok((result, _)) => Ok(result),
    Err((e, expr)) => Err((e, expr)),
  }
}

pub fn eval_str(input: &str) -> Result<String, String> {
  match parse_expr(input) {
    Ok((_, expr)) => match eval_expr(&expr) {
      Ok(result) => Ok(result.to_string()),
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
}

#[cfg(test)]
mod tests_eval {
  use std::result;

use super::*;

  #[test]
  fn test_eval_longexpr() {
    let expr = parse_expr("((f,x)=>f(f(x)))((x)=>x*6,100)").unwrap().1;
    let context = HashMap::new();
    assert_eq!(EvalResult::IVal(100*6*6), eval_expr_ctx(&expr, 0, &context).unwrap().0);
  }

  #[test]
  fn test_eval_dice() {
    let expr = parse_expr("10000d20").unwrap().1;
    let context = HashMap::new();
    match eval_expr_ctx(&expr, 0, &context) {
      Ok((EvalResult::IVal(i), _)) => {
        println!("{}", i);
        assert!(i >= 50000 && i <= 150000);
      },
      _ => assert!(false),
    }
  }

  #[test]
  fn test_eval_dice2() {
    let expr = parse_expr("1d2").unwrap().1;
    let context = HashMap::new();
    let result = eval_expr_ctx(&expr, 0, &context);
    match result {
      Ok((EvalResult::IVal(i), _)) => {
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
    let context = HashMap::new();
    match eval_expr_ctx(&expr, 0, &context) {
      Ok((EvalResult::SVal(s), _)) => {
        println!("{}", s);
      },
      _ => {
        assert!(false);
      },
    }
  }
}