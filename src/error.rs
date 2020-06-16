use std::fmt;

#[derive(Clone, Copy)]
pub struct CodePos {
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for CodePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

pub enum CompileError {
    ParseInt(CodePos),
    ParseFloat(CodePos),
}

impl std::error::Error for CompileError {}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl fmt::Debug for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ParseFloat(pos) => {
                write!(f, "CompileError: `ParseFloat` at {}", pos)
            }
            Self::ParseInt(pos) => {
                write!(f, "CompileError: `ParseInt` at {}", pos)
            }
        }
    }
}
