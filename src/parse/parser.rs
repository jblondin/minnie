use nom;

use errors::*;

use lex::{Token, Tokens, TokenType};
use lex::token;
use nom_util::CustomNomError;
use parse::ast::{Program, Statement, Identifier, Expression, Literal, Precedence};

pub struct Parser {}
impl Parser {
    pub fn parse(tokens: Vec<Token>) -> Result<Program> {
        match parse(Tokens::from_vec(&tokens)) {
            nom::IResult::Done(_, program) => Ok(program),
            nom::IResult::Incomplete(needed) => {
                let num_needed = match needed {
                    nom::Needed::Unknown => "".to_string(),
                    nom::Needed::Size(cnt) => format!("{} ", cnt),
                };
                Err(ErrorKind::Parser(format!(
                    "Incomplete parse: {}more tokens expected", num_needed)).into())
            },
            nom::IResult::Error(err) => Err(ErrorKind::Parser(
                format!("Parser failed: {}", err)).into()),
        }
    }
}

named!(parse<Tokens, Program>, many0!(parse_statement));

named!(parse_statement<Tokens, Statement>, alt!(
    parse_let_statement |
    parse_return_statement |
    parse_expression_statement
));

named!(parse_let_statement<Tokens, Statement>, do_parse!(
    tag_token!(TokenType::Let) >>
    identifier: parse_identifier >>
    tag_token!(TokenType::Equal) >>
    expression: parse_expression >>
    tag_token!(TokenType::Semicolon) >>
    (Statement::Assign(identifier, expression))
));

named!(parse_return_statement<Tokens, Statement>, do_parse!(
    tag_token!(TokenType::Return) >>
    expression: parse_expression >>
    tag_token!(TokenType::Semicolon) >>
    (Statement::Return(expression))
));

named!(parse_expression_statement<Tokens, Statement>, do_parse!(
    expression: parse_expression >>
    opt0!(tag_token!(TokenType::Semicolon)) >>
    (Statement::Expression(expression))
));

// non-operator expressions
named!(parse_unitary_expression<Tokens, Expression>, alt!(
    do_parse!(ident: call!(parse_identifier) >> (ident.into_expr())) |
    do_parse!(literal: call!(parse_literal) >> (literal.into_expr())) |
    do_parse!(expression: call!(parse_parenthetical) >> (expression))
));

named!(parse_parenthetical<Tokens, Expression>, do_parse!(
    tag_token!(TokenType::LParen) >>
    expression: parse_expression >>
    tag_token!(TokenType::RParen) >>
    (expression)
));

// start the expression parser
named!(parse_expression<Tokens, Expression>, apply!(parse_expr_precedence, Precedence::Lowest));

fn parse_expr_precedence(input: Tokens, precedence: Precedence)
        -> nom::IResult<Tokens, Expression> {
    do_parse!(input,
        left: parse_unitary_expression >>
        result: apply!(topdown, left, precedence) >>
        (result)
    )
}

fn topdown(input: Tokens, left: Expression, precedence: Precedence)
        -> nom::IResult<Tokens, Expression> {
    use nom::InputLength;

    if input.input_len() == 0 {
        // no more tokens, return
        return nom::IResult::Done(input, left);
    }
    let (rest, token) = try_parse!(input, take!(1));
    if token.input_len() == 0 {
        // no more tokens, return
        nom::IResult::Done(rest, left)
    } else {
        let peek_precedence = token.unwrap_first().ty.precedence();
        if precedence < peek_precedence {
            let expression_parser = match peek_precedence {
                Precedence::Call  => parse_call_expression,
                Precedence::Index => parse_index_expression,
                _                 => parse_infix_expression,
            };
            let (remaining, next_token) = try_parse!(input, apply!(expression_parser, left));
            topdown(remaining, next_token, precedence)
        } else {
            nom::IResult::Done(input, left)
        }
    }
}

fn parse_call_expression(input: Tokens, _: Expression) -> nom::IResult<Tokens, Expression> {
    nom::IResult::Error(error_position!(CustomNomError::Unimplemented.into(), input))
}
fn parse_index_expression(input: Tokens, _: Expression) -> nom::IResult<Tokens, Expression> {
    nom::IResult::Error(error_position!(CustomNomError::Unimplemented.into(), input))
}

fn parse_infix_expression(input: Tokens, left: Expression) -> nom::IResult<Tokens, Expression> {
    use nom::InputLength;

    let (rest, token) = try_parse!(input, take!(1));
    if token.input_len() == 0 {
        nom::IResult::Error(error_position!(CustomNomError::MissingOp.into(), input))
    } else {
        let peek_op = token.unwrap_first().ty.infix_op();
        match peek_op {
            Some(op) => {
                let (remaining, right) =
                    try_parse!(rest, apply!(parse_expr_precedence, op.precedence()));
                nom::IResult::Done(remaining, Expression::Infix(op, box left, box right))
            },
            None => {
                nom::IResult::Error(error_position!(CustomNomError::MissingOp.into(), input))
            }
        }
    }
}

named!(parse_identifier<Tokens, Identifier>, map_opt!(
    complete!(take!(1)),
    |tokens: Tokens| {
        match tokens.unwrap_first().ty {
            TokenType::Identifier(ref s) => Some(Identifier::new(s.clone())),
            _ => None
        }
    }
));

named!(parse_literal<Tokens, Literal>, map_opt!(
    complete!(take!(1)),
    |tokens: Tokens| {
        match tokens.unwrap_first().ty {
            TokenType::Literal(ref literal) => {
                match *literal {
                    token::Literal::Number(ref number_literal) => {
                        match *number_literal {
                            token::NumberLiteral::Int(i) => Some(Literal::Int(i)),
                            token::NumberLiteral::Float(f) => Some(Literal::Float(f)),
                        }
                    },
                    token::Literal::String(ref s) => Some(Literal::String(s.clone())),
                    token::Literal::Bool(b) => Some(Literal::Bool(b)),
                }
            },
            _ => None
        }
    }
));


#[cfg(test)]
mod tests {
    use super::*;
    use std::fmt::Debug;
    use lex::Lexer;
    use parse::ast::InfixOp;

    fn assert_match<T, F>(input: &str, expected: &[T], f: F)
            where F: Fn(&Statement, &T) -> bool, T: Debug {
        let tokens = Lexer::lex(input).unwrap();
        println!("tokens: {:?}", tokens);
        let program = Parser::parse(tokens).unwrap();
        println!("program: {:?}", program);
        assert_eq!(program.len(), expected.len());
        for (parsed_statement, expected_statement) in program.iter().zip(expected.iter()) {
            if !f(parsed_statement, expected_statement) {
                panic!("parsed statement {:?} does not match expected statement {:?}",
                    parsed_statement, expected_statement);
            }
        }
    }
    fn assert_program_matches(input: &str, expected: &Program) {
        assert_match(input, expected, |parsed, expctd| { parsed.eq(expctd) });
    }

    #[test]
    fn test_expression_statement() {
        let input = "aa; b; 5 ℝℨℤℤↈ";
        let expected = vec![
            Identifier::new("aa").into_expr().into_stmt(),
            Identifier::new("b").into_expr().into_stmt(),
            Literal::Int(5).into_expr().into_stmt(),
            Identifier::new("ℝℨℤℤↈ").into_expr().into_stmt(),
        ];
        assert_program_matches(input, &expected);
    }

    #[test]
    fn test_infix_expressions() {
        let input = "a + b";
        let expected = vec![
            Expression::Infix(
                InfixOp::Add,
                box Identifier::new("a").into_expr(),
                box Identifier::new("b").into_expr()
            ).into_stmt()
        ];
        assert_program_matches(input, &expected);

        let input = "a + 5.2";
        let expected = vec![
            Expression::Infix(
                InfixOp::Add,
                box Identifier::new("a").into_expr(),
                box Literal::Float(5.2).into_expr(),
            ).into_stmt()
        ];
        assert_program_matches(input, &expected);

        let input = "a + b * 5.2";
        let expected = vec![
            Expression::Infix(
                InfixOp::Add,
                box Identifier::new("a").into_expr(),
                box Expression::Infix(
                    InfixOp::Multiply,
                    box Identifier::new("b").into_expr(),
                    box Literal::Float(5.2).into_expr(),
                )
            ).into_stmt()
        ];
        assert_program_matches(input, &expected);
    }

    #[test]
    fn test_parenthetical_expressions() {
        let input = "a + (b * 5.2)";
        let expected = vec![
            Expression::Infix(
                InfixOp::Add,
                box Identifier::new("a").into_expr(),
                box Expression::Infix(
                    InfixOp::Multiply,
                    box Identifier::new("b").into_expr(),
                    box Literal::Float(5.2).into_expr(),
                )
            ).into_stmt()
        ];
        assert_program_matches(input, &expected);

        let input = "(a + b) * 5.2";
        let expected = vec![
            Expression::Infix(
                InfixOp::Multiply,
                box Expression::Infix(
                    InfixOp::Add,
                    box Identifier::new("a").into_expr(),
                    box Identifier::new("b").into_expr(),
                ),
                box Literal::Float(5.2).into_expr(),
            ).into_stmt()
        ];
        assert_program_matches(input, &expected);

        let input = "(a + b) / (c - d)";
        let expected = vec![
            Expression::Infix(
                InfixOp::Divide,
                box Expression::Infix(
                    InfixOp::Add,
                    box Identifier::new("a").into_expr(),
                    box Identifier::new("b").into_expr(),
                ),
                box Expression::Infix(
                    InfixOp::Subtract,
                    box Identifier::new("c").into_expr(),
                    box Identifier::new("d").into_expr(),
                )
            ).into_stmt()
        ];
        assert_program_matches(input, &expected);

        let input = "a + b / c - d";
        let expected = vec![
            Expression::Infix(
                InfixOp::Subtract,
                box Expression::Infix(
                    InfixOp::Add,
                    box Identifier::new("a").into_expr(),
                    box Expression::Infix(
                        InfixOp::Divide,
                        box Identifier::new("b").into_expr(),
                        box Identifier::new("c").into_expr(),
                    ),
                ),
                box Identifier::new("d").into_expr(),
            ).into_stmt()
        ];
        assert_program_matches(input, &expected);
    }

    #[test]
    fn test_let_statement() {
        let input = "let a = 5; let b = a;";
        let expected = vec![
            Statement::Assign(Identifier::new("a"), Literal::Int(5).into_expr()),
            Statement::Assign(Identifier::new("b"), Identifier::new("a").into_expr())
        ];
        assert_program_matches(input, &expected);
    }

    #[test]
    fn test_return_statement() {
        let input = r#"return 5; return a; return "fff";"#;
        let expected = vec![
            Statement::Return(Literal::Int(5).into_expr()),
            Statement::Return(Identifier::new("a").into_expr()),
            Statement::Return(Literal::string("fff").into_expr()),
        ];
        assert_program_matches(input, &expected);
    }
}