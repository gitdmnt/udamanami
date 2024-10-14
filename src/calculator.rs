use nom::{
  character::complete::{char, digit1, multispace0, alpha1, alphanumeric1, one_of},
  bytes::complete::tag,
  combinator::{map, recognize},
  sequence::{delimited, separated_pair, pair, preceded},
  multi::{separated_list0, fold_many0, many0_count},
  branch::alt,
  IResult,
};
use std::collections::HashMap;
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
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprOp1 {
  Neg,
  OneDice,
}

impl std::fmt::Display for ExprOp1 {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      ExprOp1::Neg => write!(f, "-"),
      ExprOp1::OneDice => write!(f, "d"),
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
  IVal(i64),
  FVal(f64),
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

fn parse_Apply(input: &str) -> IResult<&str, Expr> {
  map(
      pair(
        parse_term00,
          delimited(
              char('('),
              separated_list0(char(','), parse_expr),
              char(')'),
          ),
      ),
      |(term, args)| Expr::Apply(Box::new(term), args.into_iter().map(Box::new).collect()),
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

fn parse_paren(input: &str) -> IResult<&str, Expr> {
  delimited(char('('), parse_expr, char(')'))(input)
}

fn parse_term00(input: &str) -> IResult<&str, Expr> {
  preceded(multispace0, alt((
    parse_lambda,
    parse_paren,
    parse_int,
    parse_float,
    parse_named_const,
  )))(input)
}

fn parse_term0(input: &str) -> IResult<&str, Expr> {
  preceded(multispace0, alt((
    parse_Apply,
    parse_term00,
  )))(input)
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
  parse_binop_left(
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
  parse_binop_left(
    |op| alt((
      map(char('+'), |_| ExprOp2::Add),
      map(char('-'), |_| ExprOp2::Sub),
    ))(op),
    parse_term4
  )(input)
}

pub fn parse_expr(input: &str) -> IResult<&str, Expr> {
  parse_term5(input)
}




#[derive(Debug, Clone, PartialEq)]
enum EvalResult {
  IVal(i64),
  FVal(f64),
  Lambda(Vec<String>, Box<Expr>),
  Error(String),
}

#[derive(Debug, Clone, PartialEq)]
enum ResultType {
  TypeI,
  TypeF,
  TypeLambda(usize),
  TypeError,
}

#[derive(Debug, Clone, PartialEq)]
enum EvalError {
  NotANumber,
  NotAFunction,
  NegativeDice,
  InvalidDice,
  TooManyDice,
  UndefinedVar(String),
  MismatchedType(ResultType, ResultType),
  ArgCountMismatch(usize, usize),
  StepLimitExceeded,
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
      EvalError::MismatchedType(t1, t2) => write!(f, "Mismatched type: {:?} and {:?}", t1, t2),
      EvalError::ArgCountMismatch(a, b) => write!(f, "Argument count mismatch: {} and {}", a, b),
      EvalError::StepLimitExceeded => write!(f, "Step limit exceeded"),
    }
  }
}


fn eval_expr_ctx(expr: &Expr, step: usize, context: &HashMap<String, EvalResult>) -> Result<(EvalResult, usize), (EvalError, Expr)> {
  if step > 1000 {
    return Err((EvalError::StepLimitExceeded, expr.clone()));
  }
  match expr {
    Expr::IVal(i) => Ok((EvalResult::IVal(*i as i64), step)),
    Expr::FVal(f) => Ok((EvalResult::FVal(*f), step)),
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
      }
    },
    Expr::Op2(op, e1, e2) => {
      let (val1, next_step) = eval_expr_ctx(e1, step + 1, context)?;
      let (val2, next_step) = eval_expr_ctx(e2, next_step + 1, context)?;
      
      let fval1 = match val1 {
        EvalResult::IVal(i) => i as f64,
        EvalResult::FVal(f) => f,
        _ => return Err((EvalError::NotANumber, expr.clone())),
      };
      let fval2 = match val2 {
        EvalResult::IVal(i) => i as f64,
        EvalResult::FVal(f) => f,
        _ => return Err((EvalError::NotANumber, expr.clone())),
      };

      let ival1: Option<i64> = match val1 {
        EvalResult::IVal(i) => Some(i),
        _ => None,
      };
      let ival2: Option<i64> = match val2 {
        EvalResult::IVal(i) => Some(i),
        _ => None,
      };
      
      match op {
        ExprOp2::Add => match (val1, val2){(EvalResult::IVal(i1), EvalResult::IVal(i2)) => Ok((EvalResult::IVal(i1 + i2)  , next_step + 1)) , _ => Ok((EvalResult::FVal(fval1 + fval2)              , next_step + 1))},
        ExprOp2::Sub => match (val1, val2){(EvalResult::IVal(i1), EvalResult::IVal(i2)) => Ok((EvalResult::IVal(i1 - i2)  , next_step + 1)) , _ => Ok((EvalResult::FVal(fval1 - fval2)              , next_step + 1))},
        ExprOp2::Mul => match (val1, val2){(EvalResult::IVal(i1), EvalResult::IVal(i2)) => Ok((EvalResult::IVal(i1 * i2)  , next_step + 1)) , _ => Ok((EvalResult::FVal(fval1 * fval2)              , next_step + 1))},
        ExprOp2::Mod => match (val1, val2){(EvalResult::IVal(i1), EvalResult::IVal(i2)) => Ok((EvalResult::IVal(i1 % i2)  , next_step + 1)) , _ => Ok((EvalResult::IVal(fval1 as i64 % fval2 as i64), next_step + 1))},
        ExprOp2::Pow => Ok((EvalResult::FVal(fval1.powf(fval2)) , next_step + 1)),
        ExprOp2::Div => Ok((EvalResult::FVal(fval1 / fval2), next_step + 1)),
        ExprOp2::Dice => {
          let i = fval1 as i64;
          let j = fval2 as i64;
          if i < 0 {
            return Err((EvalError::NegativeDice, expr.clone()));
          }
          if j < 1 {
            return Err((EvalError::InvalidDice, expr.clone()));
          }
          if i > 10000 {
            return Err((EvalError::TooManyDice, expr.clone()));
          }
          let mut sum = 0;
          for _ in 0..i {
            sum += rand::thread_rng().gen_range(1 ..= i);
          }
          Ok((EvalResult::IVal(sum), next_step + 1))
        },
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
          let mut steps = step + 1;
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
  fn test_parse_Apply() {
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
}

#[cfg(test)]
mod tests_eval {
  use super::*;

  #[test]
  fn test_eval_longexpr() {
    let expr = parse_expr("((f,x)=>f(f(x)))((x)=>x*6,100)").unwrap().1;
    let context = HashMap::new();
    assert_eq!(EvalResult::IVal(100*6*6), eval_expr_ctx(&expr, 0, &context).unwrap().0);
  }
}