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
    do_parse!(expression: call!(parse_parenthetical) >> (expression)) |
    do_parse!(block: call!(parse_block_expression) >> (block)) |
    do_parse!(func: call!(parse_function) >> (func))
));

named!(parse_parenthetical<Tokens, Expression>, do_parse!(
    tag_token!(TokenType::LParen) >>
    expression: parse_expression >>
    tag_token!(TokenType::RParen) >>
    (expression)
));

named!(parse_block_expression<Tokens, Expression>, do_parse!(
    statements: parse_block >>
    (Expression::Block(statements))
));

named!(parse_block<Tokens, Vec<Statement>>, do_parse!(
    tag_token!(TokenType::LBrace) >>
    statements: many0!(parse_statement) >>
    tag_token!(TokenType::RBrace) >>
    (statements)
));

named!(parse_function<Tokens, Expression>, do_parse!(
    tag_token!(TokenType::Function) >>
    tag_token!(TokenType::LParen) >>
    params: parse_list0!(parse_identifier) >>
    tag_token!(TokenType::RParen) >>
    block: parse_block >>
    (Expression::Function { parameters: params, body: block })
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
    use parse::ast::InfixOp;

    if input.input_len() == 0 {
        // no more tokens, return
        return nom::IResult::Done(input, left);
    }
    let (_, token) = try_parse!(input, take!(1));
    assert!(token.input_len() > 0); // since input.input_len() > 0, this should non-empty
    let peek_token_type = &token.unwrap_first().ty;
    if precedence < peek_token_type.precedence() {
        let expression_parser = match peek_token_type.infix_op() {
            Some(InfixOp::FnCall)  => parse_call_expression,
            Some(InfixOp::Index)   => parse_index_expression,
            _                      => parse_infix_expression,
        };
        let (remaining, next_token) = try_parse!(input, apply!(expression_parser, left));
        topdown(remaining, next_token, precedence)
    } else {
        nom::IResult::Done(input, left)
    }
}

fn parse_call_expression(input: Tokens, function: Expression) -> nom::IResult<Tokens, Expression> {
    do_parse!(
        input,
        tag_token!(TokenType::LParen) >>
        fn_arguments: parse_list0!(parse_expression) >>
        tag_token!(TokenType::RParen) >>
        (Expression::FnCall { function: Box::new(function), arguments: fn_arguments })
    )
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
                nom::IResult::Done(remaining, Expression::Infix(op, Box::new(left),
                    Box::new(right)))
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
        println!("tokens: {:#?}", tokens);
        let program = Parser::parse(tokens).unwrap();
        println!("program: {:#?}", program);
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
                Box::new(Identifier::new("a").into_expr()),
                Box::new(Identifier::new("b").into_expr())
            ).into_stmt()
        ];
        assert_program_matches(input, &expected);

        let input = "a + 5.2";
        let expected = vec![
            Expression::Infix(
                InfixOp::Add,
                Box::new(Identifier::new("a").into_expr()),
                Box::new(Literal::Float(5.2).into_expr()),
            ).into_stmt()
        ];
        assert_program_matches(input, &expected);

        let input = "a + b * 5.2";
        let expected = vec![
            Expression::Infix(
                InfixOp::Add,
                Box::new(Identifier::new("a").into_expr()),
                Box::new(Expression::Infix(
                    InfixOp::Multiply,
                    Box::new(Identifier::new("b").into_expr()),
                    Box::new(Literal::Float(5.2).into_expr()),
                ))
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
                Box::new(Identifier::new("a").into_expr()),
                Box::new(Expression::Infix(
                    InfixOp::Multiply,
                    Box::new(Identifier::new("b").into_expr()),
                    Box::new(Literal::Float(5.2).into_expr()),
                ))
            ).into_stmt()
        ];
        assert_program_matches(input, &expected);

        let input = "(a + b) * 5.2";
        let expected = vec![
            Expression::Infix(
                InfixOp::Multiply,
                Box::new(Expression::Infix(
                    InfixOp::Add,
                    Box::new(Identifier::new("a").into_expr()),
                    Box::new(Identifier::new("b").into_expr()),
                )),
                Box::new(Literal::Float(5.2).into_expr()),
            ).into_stmt()
        ];
        assert_program_matches(input, &expected);

        let input = "(a + b) / (c - d)";
        let expected = vec![
            Expression::Infix(
                InfixOp::Divide,
                Box::new(Expression::Infix(
                    InfixOp::Add,
                    Box::new(Identifier::new("a").into_expr()),
                    Box::new(Identifier::new("b").into_expr()),
                )),
                Box::new(Expression::Infix(
                    InfixOp::Subtract,
                    Box::new(Identifier::new("c").into_expr()),
                    Box::new(Identifier::new("d").into_expr()),
                ))
            ).into_stmt()
        ];
        assert_program_matches(input, &expected);

        let input = "a + b / c - d";
        let expected = vec![
            Expression::Infix(
                InfixOp::Subtract,
                Box::new(Expression::Infix(
                    InfixOp::Add,
                    Box::new(Identifier::new("a").into_expr()),
                    Box::new(Expression::Infix(
                        InfixOp::Divide,
                        Box::new(Identifier::new("b").into_expr()),
                        Box::new(Identifier::new("c").into_expr()),
                    )),
                )),
                Box::new(Identifier::new("d").into_expr()),
            ).into_stmt()
        ];
        assert_program_matches(input, &expected);
    }

    #[test]
    fn test_block_expression() {
        let input = "a; {b} c";
        let expected = vec![
            Identifier::new("a").into_expr().into_stmt(),
            Expression::Block(vec![Identifier::new("b").into_expr().into_stmt()]).into_stmt(),
            Identifier::new("c").into_expr().into_stmt(),
        ];
        assert_program_matches(input, &expected);
    }

    #[test]
    fn test_function_call() {
        let input = "add(4, 2)";
        let expected = vec![
            Expression::FnCall {
                function: Box::new(Identifier::new("add").into_expr()),
                arguments: vec![
                    Literal::Int(4).into_expr(),
                    Literal::Int(2).into_expr(),
                ]
            }.into_stmt()
        ];
        assert_program_matches(input, &expected);

        let input = "add(4, 2);";
        assert_program_matches(input, &expected);

        let input = "foo(3)";
        let expected = vec![
            Expression::FnCall {
                function: Box::new(Identifier::new("foo").into_expr()),
                arguments: vec![Literal::Int(3).into_expr()],
            }.into_stmt()
        ];
        assert_program_matches(input, &expected);

        let input = "foo()";
        let expected = vec![
            Expression::FnCall {
                function: Box::new(Identifier::new("foo").into_expr()),
                arguments: vec![],
            }.into_stmt()
        ];
        assert_program_matches(input, &expected);

        let input = "foo(bar(4), baz())";
        let expected = vec![
            Expression::FnCall {
                function: Box::new(Identifier::new("foo").into_expr()),
                arguments: vec![
                    Expression::FnCall {
                        function: Box::new(Identifier::new("bar").into_expr()),
                        arguments: vec![Literal::Int(4).into_expr()],
                    },
                    Expression::FnCall {
                        function: Box::new(Identifier::new("baz").into_expr()),
                        arguments: vec![],
                    }
                ],
            }.into_stmt()
        ];
        assert_program_matches(input, &expected);
    }

    #[test]
    fn test_function_def() {
        let input = "fn(a) { return a + 2; }";
        let expected = vec![
            Expression::Function {
                parameters: vec![Identifier::new("a")],
                body: vec![
                    Statement::Return(
                        Expression::Infix(
                            InfixOp::Add,
                            Box::new(Identifier::new("a").into_expr()),
                            Box::new(Literal::Int(2).into_expr())
                        )
                    )
                ],
            }.into_stmt()
        ];
        assert_program_matches(input, &expected);

        let input = "fn(a, b) { return a + b; }";
        let expected = vec![
            Expression::Function {
                parameters: vec![
                    Identifier::new("a"),
                    Identifier::new("b"),
                ],
                body: vec![
                    Statement::Return(
                        Expression::Infix(
                            InfixOp::Add,
                            Box::new(Identifier::new("a").into_expr()),
                            Box::new(Identifier::new("b").into_expr())
                        )
                    )
                ],
            }.into_stmt()
        ];
        assert_program_matches(input, &expected);

        //TODO: implement last-expression return
        // let input = "fn(a, b) { a + b }";
        // let expected = vec![
        //     Expression::Function {
        //         parameters: vec![
        //             Identifier::new("a"),
        //             Identifier::new("b"),
        //         ],
        //         body: vec![
        //             Statement::Return(
        //                 Expression::Infix(
        //                     InfixOp::Add,
        //                     Box::new(Identifier::new("a").into_expr()),
        //                     Box::new(Identifier::new("b").into_expr())
        //                 )
        //             )
        //         ],
        //     }.into_stmt()
        // ];
        // assert_program_matches(input, &expected);

        let input = "fn() { return 2; }";
        let expected = vec![
            Expression::Function {
                parameters: vec![],
                body: vec![
                    Statement::Return(Literal::Int(2).into_expr())
                ],
            }.into_stmt()
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
