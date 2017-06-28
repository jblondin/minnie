use std::borrow::Borrow;

use lex::token::TokenType;

pub type Program = Block;
pub type Block = Vec<Statement>;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Assign(Identifier, Expression),
    Return(Expression),
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    Infix(InfixOp, Box<Expression>, Box<Expression>), // box to avoid recursive structure
    Block(Block),
}
impl Expression {
    pub fn into_stmt(self) -> Statement { Statement::Expression(self) }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Bool(bool),
    Float(f64),
    String(String),
}
impl Literal {
    pub fn into_expr(self) -> Expression { Expression::Literal(self) }
    pub fn string<T: Borrow<str>>(s: T) -> Literal {
        Literal::String(s.borrow().to_string())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String
}
impl Identifier {
    pub fn new<T: Borrow<str>>(name: T) -> Identifier {
        Identifier { name: name.borrow().to_string() }
    }
    pub fn into_expr(self) -> Expression { Expression::Identifier(self) }
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum InfixOp {
    Multiply,
    Divide,
    Add,
    Subtract,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    Equal,
    NotEqual,
}
impl InfixOp {
    pub fn precedence(&self) -> Precedence {
        match *self {
            InfixOp::Multiply | InfixOp::Divide              => Precedence::Product,
            InfixOp::Add | InfixOp::Subtract                 => Precedence::Sum,
            InfixOp::GreaterThan | InfixOp::GreaterThanEqual
                | InfixOp::LessThan | InfixOp::LessThanEqual => Precedence::LessGreater,
            InfixOp::Equal | InfixOp::NotEqual               => Precedence::Equals,
        }
    }
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    Lowest          = 1,
    Equals          = 2,
    LessGreater     = 3,
    Sum             = 4,
    Product         = 5,
    Call            = 6,
    Index           = 7,
}

impl TokenType {
    pub fn infix_op(&self) -> Option<InfixOp> {
        match *self {
            TokenType::LBracket         => None,
            TokenType::LParen           => None,
            TokenType::Asterisk         => Some(InfixOp::Multiply),
            TokenType::Slash            => Some(InfixOp::Divide),
            TokenType::Plus             => Some(InfixOp::Add),
            TokenType::Minus            => Some(InfixOp::Subtract),
            TokenType::GreaterThan      => Some(InfixOp::GreaterThan),
            TokenType::GreaterThanEqual => Some(InfixOp::GreaterThanEqual),
            TokenType::LessThan         => Some(InfixOp::LessThan),
            TokenType::LessThanEqual    => Some(InfixOp::LessThanEqual),
            TokenType::DoubleEqual      => Some(InfixOp::Equal),
            TokenType::NotEqual         => Some(InfixOp::NotEqual),
            _                           => None,
        }
    }
    pub fn precedence(&self) -> Precedence {
        // indexing or calling has no InfixOp and their own precedences.
        // everything that has an InfixOp has specific precedences to that operator
        // everything else is of lowest precedence
        match *self {
            TokenType::LBracket => Precedence::Index,
            TokenType::LParen   => Precedence::Call,
            ref ty                  => {
                match ty.infix_op() {
                    Some(io) => io.precedence(),
                    None => Precedence::Lowest,
                }
            },
        }
    }
}
