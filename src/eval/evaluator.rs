use parse::ast::{Program, Block, Statement, Expression, Literal, Identifier, InfixOp};
use eval::value::Value;
use eval::frame::Frame;

pub struct Evaluator {
    frame: Frame,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            frame: Frame::new(),
        }
    }

    pub fn evaluate(&mut self, program: Program) -> Value {
        self.eval_block(program).ret()
    }

    pub fn eval_block(&mut self, mut block: Block) -> Value {
        // need to reverse it since we use 'pop' (could also perhaps use a VecDeque, or a draining
        // iterator, or statement cloning)
        block.reverse();
        self.eval_reversed_block(block)
    }

    pub fn eval_reversed_block(&mut self, mut block: Block) -> Value {
        if let Some(first) = block.pop() {
            let value = self.eval_statement(first);
            if value.is_return() || block.is_empty() {
                value
            } else {
                self.eval_reversed_block(block)
            }
        } else {
            Value::Empty
        }
    }

    pub fn eval_statement(&mut self, statement: Statement) -> Value {
        match statement {
            Statement::Assign(ident, expr) => {
                let value = self.eval_expression(expr);
                self.frame.set(ident.clone(), value);
                Value::Empty
            },
            Statement::Return(expr)        => Value::Return(Box::new(self.eval_expression(expr))),
            Statement::Expression(expr)    => self.eval_expression(expr),
        }
    }

    pub fn eval_expression(&mut self, expr: Expression) -> Value {
        match expr {
            Expression::Identifier(ident)       => self.eval_identifier(ident),
            Expression::Literal(literal)        => self.eval_literal(literal),
            Expression::Infix(op, left, right)  => self.eval_infix(op, *left, *right),
            Expression::Block(block)            => self.eval_block(block),
            Expression::Function { parameters, body }
                                                => self.eval_function_def(parameters, body),
            Expression::FnCall { function, arguments }
                                                => self.eval_function_call(*function, arguments),
        }
    }

    pub fn eval_identifier(&mut self, identifier: Identifier) -> Value {
        match self.frame.get(&identifier) {
            Some(value) => value,
            None        => Value::Error
        }
    }

    pub fn eval_literal(&mut self, literal: Literal) -> Value {
        match literal {
            Literal::Int(i)    => Value::Integer(i),
            Literal::Float(f)  => Value::Float(f),
            Literal::Bool(b)   => Value::Bool(b),
            Literal::String(s) => Value::String(s),
        }
    }

    pub fn eval_infix(&mut self, op: InfixOp, left: Expression, right: Expression) -> Value {
        let left_value = self.eval_expression(left);
        let right_value = self.eval_expression(right);

        match op {
            InfixOp::Add      => add_values(left_value, right_value),
            InfixOp::Subtract => subtract_values(left_value, right_value),
            InfixOp::Multiply => multiply_values(left_value, right_value),
            InfixOp::Divide   => divide_values(left_value, right_value),
            _                 => Value::Unimplemented,
        }
    }

    pub fn eval_function_def(&mut self, params: Vec<Identifier>, body: Block) -> Value {
        Value::Function {
            parameters: params,
            body: body,
            context: self.frame.clone(),
        }
    }

    pub fn eval_function_call(&mut self, func: Expression, args: Vec<Expression>) -> Value {
        let function_value = self.eval_expression(func);
        if let Value::Function { parameters, body, context } = function_value {
            if args.len() != parameters.len() { return Value::Error; }
            let mut args_evald: Vec<Value> = args.iter()
                .map(|expr| self.eval_expression(expr.clone())).collect();
            // save caller frame
            let caller_frame = self.frame.clone();
            // create a new frame out of the context of where the function is defined
            let mut callee_frame = Frame::push(&context);
            for (identifier, value) in parameters.iter().zip(args_evald.drain(..)) {
                callee_frame.set(identifier.clone(), value);
            }
            //FIXME: I really don't like this, should move to a more functional method,
            // where I pass the frame around, but that may be bulky too... maybe the frame contains
            // the evaluator instead?
            self.frame = callee_frame;
            let value = self.eval_block(body);
            self.frame = caller_frame;
            value.ret()
        } else {
            Value::Error
        }
    }
}

//TODO: create separate 'infix' module
fn add_values(left: Value, right: Value) -> Value {
    match (left, right) {
        (Value::Integer(l), Value::Integer(r)) => Value::Integer(l + r),
        (Value::Integer(l), Value::Float(r)  ) => Value::Float(l as f64 + r),
        (Value::Float(l),   Value::Integer(r)) => Value::Float(l + r as f64),
        (Value::Float(l),   Value::Float(r)  ) => Value::Float(l + r),
        (Value::String(l),  Value::String(r) ) => Value::String(l + &r),
        (_,                 _                ) => Value::Unimplemented,
    }
}

fn subtract_values(left: Value, right: Value) -> Value {
    match (left, right) {
        (Value::Integer(l), Value::Integer(r)) => Value::Integer(l - r),
        (Value::Integer(l), Value::Float(r)  ) => Value::Float(l as f64 - r),
        (Value::Float(l),   Value::Integer(r)) => Value::Float(l - r as f64),
        (Value::Float(l),   Value::Float(r)  ) => Value::Float(l - r),
        (_,                 _                ) => Value::Unimplemented,
    }
}

fn multiply_values(left: Value, right: Value) -> Value {
    match (left, right) {
        (Value::Integer(l), Value::Integer(r)) => Value::Integer(l * r),
        (Value::Integer(l), Value::Float(r)  ) => Value::Float(l as f64 * r),
        (Value::Float(l),   Value::Integer(r)) => Value::Float(l * r as f64),
        (Value::Float(l),   Value::Float(r)  ) => Value::Float(l * r),
        (_,                 _                ) => Value::Unimplemented,
    }
}

fn divide_values(left: Value, right: Value) -> Value {
    match (left, right) {
        (Value::Integer(l), Value::Integer(r)) => Value::Float(l as f64 / r as f64),
        (Value::Integer(l), Value::Float(r)  ) => Value::Float(l as f64 / r),
        (Value::Float(l),   Value::Integer(r)) => Value::Float(l / r as f64),
        (Value::Float(l),   Value::Float(r)  ) => Value::Float(l / r),
        (_,                 _                ) => Value::Unimplemented,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lex::Lexer;
    use parse::Parser;

    fn assert_value_matches(input: &str, expected: Value) {
        let tokens = Lexer::lex(input).unwrap();
        println!("tokens: {:?}", tokens);
        let program = Parser::parse(tokens).unwrap();
        println!("program: {:?}", program);
        let value = Evaluator::new().evaluate(program);
        println!("value: {:#?}", value);
        println!("expected: {:#?}", expected);
        match value {
            Value::Function { parameters, body, .. } => {
                if let Value::Function { parameters: exp_params, body: exp_body, .. } = expected {
                    // ignore context
                    assert_eq!(parameters, exp_params);
                    assert_eq!(body, exp_body);
                } else {
                    panic!("Expected Value::Function, but found: {:?}", expected);
                }
            },
            other => { assert_eq!(other, expected); }
        }
    }

    #[test]
    fn test_infix_literals() {
        assert_value_matches("5 + 3", Value::Integer(8));
        assert_value_matches("5 + (3 * 4)", Value::Integer(17));
        assert_value_matches("5 + 3 * 4", Value::Integer(17));
        assert_value_matches("(5 + 3) * 4", Value::Integer(32));
        assert_value_matches("5 / 2", Value::Float(2.5));
        assert_value_matches("4 / 2", Value::Float(2.0));
        assert_value_matches("5 - 2", Value::Integer(3));
        assert_value_matches("5 - 2.0", Value::Float(3.0));
        assert_value_matches("5.0 - 2", Value::Float(3.0));
        assert_value_matches("5.0 - 2.0", Value::Float(3.0));
        assert_value_matches("5 + 3 / 4 - 2", Value::Float(3.75));
        assert_value_matches("(5 + 3) / (4 - 2)", Value::Float(4.0));
    }

    #[test]
    fn test_return_literals() {
        assert_value_matches("5 + 3; return 3 + 2; 19", Value::Integer(5));
        assert_value_matches("5 + 3; return 3 + 2;", Value::Integer(5));
        assert_value_matches("5 + 3; 3 + 2", Value::Integer(5));
        assert_value_matches("5 + 3; 3 + 2;", Value::Integer(5));
        assert_value_matches("return 5 + 3; 3 + 2", Value::Integer(8));
        assert_value_matches("return 5 + 3; 3 + 2;", Value::Integer(8));
        assert_value_matches("return (5 + 3); (3 + 2)", Value::Integer(8));
    }

    #[test]
    fn test_block_expression() {
        assert_value_matches("{5 + 3}", Value::Integer(8));
        assert_value_matches("{5 + 3;}", Value::Integer(8));
        assert_value_matches("{5 + 3;};", Value::Integer(8));
        assert_value_matches("5 + 3; { 3 + 2 }", Value::Integer(5));
        assert_value_matches("5 + 3; return { 3 + 2 };", Value::Integer(5));
        assert_value_matches("5 + 3; { return 3 + 2; }; 2 + 4", Value::Integer(5));
        assert_value_matches("5 + 3; return { 3 + 2 }; 2 + 4", Value::Integer(5));
    }

    #[test]
    fn test_assign_statement() {
        assert_value_matches("let a = 5; a", Value::Integer(5));
        assert_value_matches("let a = 5; a + 2", Value::Integer(7));
        assert_value_matches("let a = 5; let b = 2; 5 + 2", Value::Integer(7));
    }

    #[test]
    fn test_function_def() {
        assert_value_matches("let add2 = fn(a) { return a + 2; }; add2",
            Value::Function {
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
                context: Frame::new(),
            }
        );
    }
}
