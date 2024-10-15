use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case},
    character::complete::{space0, u128, u32, u64},
    combinator::{opt, value},
    error::{Error, ErrorKind},
    sequence::{separated_pair, tuple},
    Finish, IResult,
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
    let (remaining_input, output) = tuple((
        separated_pair(u32, tag_no_case("d"), u64),
        opt(tuple((space0, parse_cmp_operator, space0, u128))),
    ))(input)?;
    let ((num, dice), cmp) = output;
    if let Some((_, operator, _, operand)) = cmp {
        Ok((remaining_input, (num, dice, Some((operator, operand)))))
    } else {
        Ok((remaining_input, (num, dice, None)))
    }
}
