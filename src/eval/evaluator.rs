use parse::ast::{Program, Block, Statement, Expression, Literal, Identifier, InfixOp};
use eval::value::Value;

pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Evaluator { Evaluator {} }

    pub fn evaluate(&mut self, program: Program) -> Value {
        self.eval_block(program)
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
                value.ret()
            } else {
                self.eval_block(block)
            }
        } else {
            Value::Empty
        }
    }

    pub fn eval_statement(&mut self, statement: Statement) -> Value {
        match statement {
            Statement::Assign(_, _)     => Value::Unimplemented,
            Statement::Return(expr)     => Value::Return(box self.eval_expression(expr)),
            Statement::Expression(expr) => self.eval_expression(expr),
        }
    }

    pub fn eval_expression(&mut self, expr: Expression) -> Value {
        match expr {
            Expression::Identifier(ident)      => self.eval_identifier(ident),
            Expression::Literal(literal)       => self.eval_literal(literal),
            Expression::Infix(op, left, right) => self.eval_infix(op, *left, *right),
        }
    }

    pub fn eval_identifier(&mut self, _: Identifier) -> Value {
        Value::Unimplemented
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
        println!("value: {:?}", value);
        assert_eq!(value, expected);
    }

    #[test]
    fn test_infix_literals() {
        let input = "5 + 3";
        let expected = Value::Integer(8);
        assert_value_matches(input, expected);
    }
}
