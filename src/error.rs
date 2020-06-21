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
    UnexpectedOperator(Option<CodePos>, String),
    UnexpectedBinaryOperator(Option<CodePos>, String),
    UnexpectedUnaryOperator(Option<CodePos>, String),
    UnknownOperator(Option<CodePos>, String),
    ExpectedBinaryOperator(Option<CodePos>),
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
            Self::UnclosedStringLiteral(pos) => {
                write!(f, "Unclosed string literal at {}.", pos)
            }
            Self::UnexpectedOperator(pos, op) => {
                write!(f, "Unexpected operator '{}' at {:?}.", op, pos)
            }
            Self::UnexpectedBinaryOperator(pos, op) => {
                write!(f, "Unexpected binary operator '{}' at {:?}.", op, pos)
            }
            Self::ExpectedBinaryOperator(pos) => {
                write!(f, "Expected binary operator at {:?}.", pos)
            }
            Self::UnknownOperator(pos, op) => {
                write!(f, "Unknown operator '{}' at {:?}.", op, pos)
            }
            Self::UnexpectedUnaryOperator(pos, op) => {
                write!(f, "Unexpected unary operator '{}' at {:?}.", op, pos)
            }
        }
    }
}

pub enum RuntimeError {
    MismatchedParameterCount(usize, usize),
    MismatchedDataTypes(DataType, DataType),
    ExpectedNumber(DataType),
    ExpectedSignedNumber(DataType),
    ExpectedInteger(DataType),
    NotCallableDataType(DataType),
    CallListNotRight,
    IncorrectListLambdaSyntax,
    VarIsNotDefined(String),
    ConvertError(DataType, DataType),
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
            Self::MismatchedParameterCount(a, b) => write!(
                f,
                "Expected {} params but {} params were supplied.",
                a, b
            ),
            Self::NotCallableDataType(data_type) => {
                write!(f, "{} is not callable.", data_type)
            }
            Self::MismatchedDataTypes(expected, found) => write!(
                f,
                "Expected the data type {} but found {}.",
                expected, found
            ),
            Self::CallListNotRight => write!(
                f,
                "Lists are not callable unless used for indexing or creating lambdas."
            ),
            Self::IncorrectListLambdaSyntax => {
                write!(f, "Lambda argument list has incorrect structure.")
            }
            Self::VarIsNotDefined(var) => {
                write!(f, "The variable `{}` isn't defined.", var)
            }
            Self::ConvertError(into, from) => write!(f, "Can't convert {} into {}.", from, into),
            Self::ExpectedNumber(found) => write!(f, "Expected number but found {}.", found),
            Self::ExpectedSignedNumber(found) => write!(f, "Expected signed number but found {}.", found),
            Self::ExpectedInteger(found) => write!(f, "Expected an integer type but found {}.", found),
        }
    }
}
