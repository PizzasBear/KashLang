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
        }
    }
}

pub enum RuntimeError {
    MismatchedParameterCount(CodePos, usize, usize),
    MismatchedDataTypes(CodePos, DataType, DataType),
    ExpectedNumber(CodePos, DataType),
    ExpectedSignedNumber(CodePos, DataType),
    ExpectedInteger(CodePos, DataType),
    CallListNotRight(CodePos),
    IncorrectListLambdaSyntax(CodePos),
    VarIsNotDefined(CodePos, String),
    ConvertError(CodePos, DataType, DataType),
    RedefinedVariableInTheSameScope(CodePos, String),
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
            Self::MismatchedParameterCount(pos, a, b) => write!(
                f,
                "Expected {} params but {} params were supplied at {}.",
                a, b, pos
            ),
            Self::MismatchedDataTypes(pos, expected, found) => write!(
                f,
                "Expected the data type {} but found {} at {}.",
                expected, found, pos
            ),
            Self::CallListNotRight(pos) => write!(
                f,
                "Called list not for indexing, creating lambdas or data duplication at {}.",
                pos
            ),
            Self::IncorrectListLambdaSyntax(pos) => write!(
                f,
                "Lambda argument list has incorrect structure at {}.",
                pos
            ),
            Self::VarIsNotDefined(pos, var) => {
                write!(f, "The variable `{}` isn't defined at {}.", var, pos)
            }
            Self::ConvertError(pos, into, from) => {
                write!(f, "Can't convert {} into {} at {}.", from, into, pos)
            }
            Self::ExpectedNumber(pos, found) => {
                write!(f, "Expected number but found {} at {}.", found, pos)
            }
            Self::ExpectedSignedNumber(pos, found) => {
                write!(f, "Expected signed number but found {} at {}.", found, pos)
            }
            Self::ExpectedInteger(pos, found) => write!(
                f,
                "Expected an integer type but found {} at {}.",
                found, pos
            ),
            Self::RedefinedVariableInTheSameScope(pos, name) => write!(
                f,
                "Redefined variable '{}' at the same scope it was defined in, at {}.",
                name, pos
            ),
        }
    }
}
