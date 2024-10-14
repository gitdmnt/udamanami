use nom::{
  character::complete::{char, digit1, multispace0, multispace1, alphanumeric1, alpha1},
  combinator::map,
  sequence::{delimited, separated_pair},
  IResult,
};

enum Expr {
  Val(i32),
  NamedConst(String),
  Add(Box<Expr>, Box<Expr>),
  Sub(Box<Expr>, Box<Expr>),
  Mul(Box<Expr>, Box<Expr>),
  Div(Box<Expr>, Box<Expr>),
  Mod(Box<Expr>, Box<Expr>),
  Dice(Box<Expr>, Box<Expr>),
  Exp(Box<Expr>, Box<Expr>),
  FunCall(String, Vec<Expr>),
}


/*
演算子の結合性と優先順位
1. 無結合 D
2. 右結合 ^
3. 左結合 * / %
4. 左結合 + -
*/

fn parse_int(input: &str) -> IResult<&str, Expr> {
  map(digit1, |s: &str| Expr::Val(s.parse().unwrap()))(input)
}

fn parse_float(input: &str) -> IResult<&str, Expr> {
  map(
    separated_pair(digit1, char('.'), digit1),
    |(a, b): (&str, &str)| Expr::Val(format!("{}.{}", a, b).parse().unwrap()),
  )(input)
}

// "e"とか"pi"とか。存在チェックは計算時にやる
fn parse_named_const(input: &str) -> IResult<&str, Expr> {
  map(
    alpha1,
    |s: &str| Expr::NamedConst(s.to_string()),
  )(input)
}

fn parse_fncall(input: &str) -> IResult<&str, Expr> {
  map(
    separated_pair(
      alphanumeric1,
      char('('),
      delimited(
        multispace0,
        separated_pair(parse_expr, char(','), multispace0),
        char(')'),
      ),
    ),
    |(name, args)| Expr::FunCall(name.to_string(), args),
  )(input)
}

// 0: 関数呼び出し・定数・括弧