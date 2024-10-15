use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case},
    character::complete::{digit1, space0},
    combinator::{map_res, opt, value},
    sequence::{separated_pair, tuple},
    IResult, Parser,
};

#[derive(Clone)]
pub enum CmpOperator {
    Equal,
    GreaterEqual,
    GreaterThan,
    LessEqual,
    LessThan,
    NotEqual,
}

impl Into<&str> for CmpOperator {
    fn into(self) -> &'static str {
        match self {
            CmpOperator::Equal => "==",
            CmpOperator::GreaterEqual => ">=",
            CmpOperator::GreaterThan => ">",
            CmpOperator::LessEqual => "<=",
            CmpOperator::LessThan => "<",
            CmpOperator::NotEqual => "!=",
        }
    }
}

pub fn cmp_with_operator(operator: &CmpOperator, left: u128, right: u128) -> bool {
    match operator {
        CmpOperator::Equal => left == right,
        CmpOperator::GreaterEqual => left >= right,
        CmpOperator::GreaterThan => left > right,
        CmpOperator::LessEqual => left <= right,
        CmpOperator::LessThan => left < right,
        CmpOperator::NotEqual => left != right,
    }
}

fn parse_cmp_operator(input: &str) -> IResult<&str, CmpOperator> {
    alt((
        value(CmpOperator::Equal, alt((tag("=="), tag("=")))),
        value(CmpOperator::GreaterEqual, tag(">=")),
        value(CmpOperator::GreaterThan, tag(">")),
        value(CmpOperator::LessEqual, tag("<=")),
        value(CmpOperator::LessThan, tag("<")),
        value(CmpOperator::NotEqual, tag("!=")),
    ))(input)
}

pub fn parse_dice(input: &str) -> IResult<&str, (u32, u64, Option<(CmpOperator, u128)>)> {
    tuple((
        separated_pair(
            map_res(digit1, str::parse),
            tag_no_case("d"),
            map_res(digit1, str::parse),
        ),
        opt(tuple((
            space0,
            parse_cmp_operator,
            space0,
            map_res(digit1, str::parse),
        ))),
    ))
    .map(|((num, dice), cmp)| (num, dice, cmp.map(|(_, op, _, operand)| (op, operand))))
    .parse(input)
}