use std::error::Error;
use std::fmt;

use crate::interpreter::DataType;

#[derive(Clone, Copy)]
pub struct CodePos {
    pub line: usize,
    pub column: usize,
}

impl PartialEq for CodePos {
    fn eq(&self, other: &Self) -> bool {
        self.line == other.line && self.column == other.column
    }
}

impl Eq for CodePos {}

impl fmt::Display for CodePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl fmt::Debug for CodePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

pub enum CompileError {
    ParseInt(CodePos),
    ParseFloat(CodePos),
    MismatchedClosingDelimiter(CodePos, char, char),
    UnexpectedClosingDelimiter(CodePos, char),
    UnclosedStringLiteral(CodePos),
    UnexpectedOperator(CodePos, String),
    UnexpectedBinaryOperator(CodePos, String),
    UnexpectedUnaryOperator(CodePos, String),
    UnknownOperator(CodePos, String),
    ExpectedBinaryOperator(CodePos),
    EmptyBlock(CodePos),
}

impl Error for CompileError {}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl fmt::Debug for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Compile: ")?;
        match self {
            Self::ParseFloat(pos) => write!(f, "`ParseFloat` at {}.", pos),
            Self::ParseInt(pos) => write!(f, "`ParseInt` at {}.", pos),
            Self::UnexpectedClosingDelimiter(pos, ch) => {
                write!(f, "Unexpected closing delimiter '{}' at {}.", ch, pos)
            }
            Self::MismatchedClosingDelimiter(pos, open, close) => write!(
                f,
                "Mismatched closing delimiter '{}{}' at {}.",
                open, close, pos
            ),
            Self::UnclosedStringLiteral(pos) => write!(f, "Unclosed string literal at {}.", pos),
            Self::UnexpectedOperator(pos, op) => {
                write!(f, "Unexpected operator '{}' at {}.", op, pos)
            }
            Self::UnexpectedBinaryOperator(pos, op) => {
                write!(f, "Unexpected binary operator '{}' at {}.", op, pos)
            }
            Self::ExpectedBinaryOperator(pos) => write!(f, "Expected binary operator at {}.", pos),
            Self::UnknownOperator(pos, op) => write!(f, "Unknown operator '{}' at {}.", op, pos),
            Self::UnexpectedUnaryOperator(pos, op) => {
                write!(f, "Unexpected unary operator '{}' at {}.", op, pos)
            }
            Self::EmptyBlock(pos) => write!(f, "The block at {} didn't return any value", pos),
        }
    }
}

pub enum RuntimeError {
    MismatchedParameterCount(CodePos, usize, usize, bool),
    MismatchedDataTypes(CodePos, Vec<DataType>, DataType),
    MismatchedReturnDataTypes(CodePos, Vec<DataType>, DataType),
    VarIsNotDefined(CodePos, String),
    ConvertError(CodePos, DataType, DataType),
    RedefinedVariableInTheSameScope(CodePos, String),
    Raised(CodePos, String),
}

impl Error for RuntimeError {}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl fmt::Debug for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Runtime: ")?;
        match self {
            Self::MismatchedParameterCount(pos, expected, found, etc) => {
                if *etc {
                    write!(
                        f,
                        "Expected at least {} params but {} params were supplied at {}.",
                        expected, found, pos
                    )
                } else {
                    write!(
                        f,
                        "Expected {} params but {} params were supplied at {}.",
                        expected, found, pos
                    )
                }
            }
            Self::MismatchedDataTypes(pos, expected, found) => write!(
                f,
                "Expected one of the types {:?} but found {} at {}.",
                expected, found, pos
            ),
            Self::MismatchedReturnDataTypes(pos, expected, found) => write!(
                f,
                "Expected that the lambda at {} will return one of the types {:?} but found {}.",
                pos, expected, found
            ),
            Self::VarIsNotDefined(pos, var) => {
                write!(f, "The variable `{}` isn't defined at {}.", var, pos)
            }
            Self::ConvertError(pos, into, from) => {
                write!(f, "Can't convert {} into {} at {}.", from, into, pos)
            }
            Self::RedefinedVariableInTheSameScope(pos, name) => write!(
                f,
                "Redefined variable '{}' at the same scope it was defined in, at {}.",
                name, pos
            ),
            Self::Raised(pos, msg) => write!(f, "Raised: `{}` at {}", msg, pos),
        }
    }
}
