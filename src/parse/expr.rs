#[derive(Clone, Copy)]
pub enum BinaryOperator {
    Add = 0x00,
    Sub = 0x01,
    Mul = 0x10,
    Div = 0x11,
}

impl BinaryOperator {
    pub fn from_char(ch: char) -> Option<Self> {
        match ch {
            '+' => Some(Self::Add),
            '-' => Some(Self::Sub),
            '*' => Some(Self::Mul),
            '/' => Some(Self::Div),
            _ => None,
        }
    }

    pub fn order(&self) -> usize {
        *self as usize >> 4
    }
}

#[derive(Clone, Copy)]
pub enum UnaryOperator {
    Not = 0x30,
}

impl UnaryOperator {
    pub fn from_char(ch: char) -> Option<Self> {
        match ch {
            '!' => Some(Self::Not),
            _ => None,
        }
    }

    pub fn order(&self) -> usize {
        *self as usize >> 4
    }
}

#[derive(Clone, Debug)]
pub enum Literal {
    Str(String),
    Int(i32),
    UInt(u32),
    Float(f32),
}

enum Expr {
    Fn(usize, Box<Expr>),
    Unary(UnaryOperator, Box<Expr>),
    Binary(BinaryOperator, Box<Expr>, Box<Expr>),
    Group(Box<Expr>),
    Tuple(Vec<Expr>),
    List(Vec<Expr>),
    Literal(Literal),
    None,
}
