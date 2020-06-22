use std::fmt;

use crate::CompileError;
use crate::error::CodePos;

#[derive(Clone, Copy)]
pub enum Operator {
    Neg = 0x00,
    Not = 0x01,
    Mul = 0x10,
    Div = 0x11,
    Mod = 0x12,
    Add = 0x20,
    Sub = 0x21,
    ShiftLeft = 0x30,
    ShiftRight = 0x31,
    Less = 0x40,
    Greater = 0x41,
    LessEq = 0x42,
    GreaterEq = 0x43,
    Equals = 0x50,
    NotEq = 0x51,
    BitAnd = 0x60,
    BitXor = 0x70,
    BitOr = 0x80,
    And = 0x90,
    Or = 0xa0,
}

impl Operator {
    pub fn order(&self) -> usize {
        *self as usize >> 4
    }

    pub fn is_unary(&self) -> bool {
        self.order() == 0
    }

    pub fn is_binary(&self) -> bool {
        !self.is_unary()
    }

    pub fn to_fn(&self) -> &'_ str {
        match self {
            Self::Neg => "neg",
            Self::Not => "not",
            Self::Mul => "mul",
            Self::Div => "div",
            Self::Mod => "mod",
            Self::Add => "add",
            Self::Sub => "sub",
            Self::ShiftLeft => "shift_left",
            Self::ShiftRight => "shift_right",
            Self::Less => "less",
            Self::Greater => "greater",
            Self::LessEq => "less_eq",
            Self::GreaterEq => "greater_eq",
            Self::Equals => "eq",
            Self::NotEq => "diff",
            Self::BitAnd => "bit_and",
            Self::BitXor => "bit_xor",
            Self::BitOr => "bit_or",
            Self::And => "and",
            Self::Or => "or",
        }
    }
}

impl<'a> std::convert::TryFrom<(CodePos, &String, bool)> for Operator {
    type Error = CompileError;
    fn try_from((pos, s, is_binary): (CodePos, &String, bool)) -> Result<Self, Self::Error> {
        if is_binary {
            match s.as_str() {
                "*" => Ok(Self::Mul),
                "/" => Ok(Self::Div),
                "%" => Ok(Self::Mod),
                "+" => Ok(Self::Add),
                "-" => Ok(Self::Sub),
                "<<" => Ok(Self::ShiftLeft),
                ">>" => Ok(Self::ShiftRight),
                "<" => Ok(Self::Less),
                ">" => Ok(Self::Greater),
                "<=" => Ok(Self::LessEq),
                ">=" => Ok(Self::GreaterEq),
                "=" => Ok(Self::Equals),
                "!=" => Ok(Self::NotEq),
                "&" => Ok(Self::BitAnd),
                "^" => Ok(Self::BitXor),
                "|" => Ok(Self::BitOr),
                "&&" => Ok(Self::And),
                "||" => Ok(Self::Or),
                "!" => Err(CompileError::UnexpectedBinaryOperator(pos, s.clone())),
                _ => Err(CompileError::UnknownOperator(pos, s.clone())),
            }
        } else {
            match s.as_str() {
                "-" => Ok(Self::Neg),
                "!" => Ok(Self::Not),
                _ => Err(CompileError::UnexpectedOperator(pos, s.clone())),
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Literal {
    Str(String),
    Int(i32),
    UInt(u32),
    Float(f32),
}

#[derive(Clone)]
pub struct Expr(pub CodePos, pub ExprType);

#[derive(Clone)]
pub enum ExprType {
    Literal(Literal),
    Lambda(Vec<Expr>),
    FnCall(Box<Expr>, Vec<Expr>),
    Scope(Vec<Expr>),
    List(Vec<Expr>),
    Var(String),
}

fn write_indent(f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
    for _ in 0..indent {
        write!(f, "\t")?;
    }
    Ok(())
}

impl Expr {
    fn fmt_indent(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        match &self.1 {
            ExprType::Literal(literal) => {
                write!(f, "LITERAL ")?;
                match literal {
                    Literal::Int(i) => write!(f, "{}", i),
                    Literal::UInt(u) => write!(f, "{}u", u),
                    Literal::Float(fl) => write!(f, "{}f", fl),
                    Literal::Str(s) => write!(f, "{:?}", s),
                }?;
            }
            ExprType::Lambda(lines) => {
                writeln!(f, "LAMBDA {{")?;
                for expr in lines.iter() {
                    write_indent(f, indent + 1)?;
                    expr.fmt_indent(f, indent + 1)?;
                    writeln!(f, ";")?;
                }
                write_indent(f, indent)?;
                write!(f, "}}")?;
            }
            ExprType::Scope(lines) => {
                writeln!(f, "SCOPE (")?;
                for expr in lines.iter() {
                    write_indent(f, indent + 1)?;
                    expr.fmt_indent(f, indent + 1)?;
                    writeln!(f, ";")?;
                }
                write_indent(f, indent)?;
                write!(f, ")")?;
            }
            ExprType::FnCall(fn_expr, fn_args) => {
                write!(f, "CALL ")?;
                fn_expr.fmt_indent(f, indent)?;
                writeln!(f, " WITH [")?;
                for arg in fn_args.iter() {
                    write_indent(f, indent + 1)?;
                    arg.fmt_indent(f, indent + 1)?;
                    writeln!(f, ",")?;
                }
                write_indent(f, indent)?;
                write!(f, "]")?;
            }
            ExprType::List(list) => {
                writeln!(f, "LIST [")?;
                for arg in list.iter() {
                    write_indent(f, indent + 1)?;
                    arg.fmt_indent(f, indent + 1)?;
                    writeln!(f, ",")?;
                }
                write_indent(f, indent)?;
                write!(f, "]")?;
            }
            ExprType::Var(var) => {
                write!(f, "VARIABLE {}", var)?;
            }
        }
        Ok(())
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_indent(f, 0)
    }
}
