use std::borrow::Borrow;

use lex::token::Token;

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
    Function {
        parameters: Vec<Identifier>,
        body: Block,
    },
    FnCall {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
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

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
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
    FnCall,
    Index,
}
impl InfixOp {
    pub fn precedence(&self) -> Precedence {
        match *self {
            InfixOp::Index                                   => Precedence::Index,
            InfixOp::FnCall                                  => Precedence::Call,
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

impl Token {
    pub fn infix_op(&self) -> Option<InfixOp> {
        match *self {
            Token::LBracket         => Some(InfixOp::Index),
            Token::LParen           => Some(InfixOp::FnCall),
            Token::Asterisk         => Some(InfixOp::Multiply),
            Token::Slash            => Some(InfixOp::Divide),
            Token::Plus             => Some(InfixOp::Add),
            Token::Minus            => Some(InfixOp::Subtract),
            Token::GreaterThan      => Some(InfixOp::GreaterThan),
            Token::GreaterThanEqual => Some(InfixOp::GreaterThanEqual),
            Token::LessThan         => Some(InfixOp::LessThan),
            Token::LessThanEqual    => Some(InfixOp::LessThanEqual),
            Token::DoubleEqual      => Some(InfixOp::Equal),
            Token::NotEqual         => Some(InfixOp::NotEqual),
            _                           => None,
        }
    }
    pub fn precedence(&self) -> Precedence {
        // indexing or calling has no InfixOp and their own precedences.
        // everything that has an InfixOp has specific precedences to that operator
        // everything else is of lowest precedence
        match self.infix_op() {
            Some(io) => io.precedence(),
            None => Precedence::Lowest,
        }
    }
}
