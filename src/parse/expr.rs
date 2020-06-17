#[derive(Clone, Copy)]
pub enum BinaryOperator {
    Access = 0x00,
    Mul = 0x20,
    Div = 0x21,
    Mod = 0x22,
    Add = 0x30,
    Sub = 0x31,
    ShiftLeft = 0x40,
    ShiftRight = 0x41,
    Less = 0x50,
    Greater = 0x51,
    LessEq = 0x52,
    GreaterEq = 0x53,
    Equals = 0x60,
    NotEq = 0x61,
    BitAnd = 0x70,
    BitXor = 0x80,
    BitOr = 0x90,
    And = 0xa0,
    Or = 0xb0,
    Set = 0xc0,
    AddSet = 0xc1,
    SubSet = 0xc2,
    MulSet = 0xc3,
    DivSet = 0xc4,
    ModSet = 0xc5,
    ShiftLeftSet = 0xc6,
    ShiftRightSet = 0xc7,
    BitAndSet = 0xc8,
    BitXorSet = 0xc9,
    BitOrSet = 0xca,
    Comma = 0xd0,
}
#[derive(Clone, Copy)]
pub enum UnaryOperator {
    Minus = 0x10,
    Not = 0x11,
    Deref = 0x12,
    Mem = 0x13,
}

impl BinaryOperator {
    pub fn order(&self) -> usize {
        *self as usize >> 4
    }
}
impl UnaryOperator {
    pub fn order(&self) -> usize {
        *self as usize >> 4
    }
}

impl std::convert::TryFrom<String> for BinaryOperator {
    type Error = ();
    fn try_from(s: String) -> Result<Self, Self::Error> {
        use std::convert::TryInto;
        s.as_str().try_into()
    }
}
impl std::convert::TryFrom<String> for UnaryOperator {
    type Error = ();
    fn try_from(s: String) -> Result<Self, Self::Error> {
        use std::convert::TryInto;
        s.as_str().try_into()
    }
}

impl<'a> std::convert::TryFrom<&'a str> for BinaryOperator {
    type Error = ();
    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        match s {
            "+" => Ok(Self::Add),
            "-" => Ok(Self::Sub),
            "*" => Ok(Self::Mul),
            "/" => Ok(Self::Div),
            _ => Err(()),
        }
    }
}
impl<'a> std::convert::TryFrom<&'a str> for UnaryOperator {
    type Error = ();
    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        match s {
            "!" => Ok(Self::Not),
            _ => Err(()),
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

pub enum Expr {
    Fn(usize, Box<Expr>),
    Unary(UnaryOperator, Box<Expr>),
    Binary(BinaryOperator, Box<Expr>, Box<Expr>),
    Group(Box<Expr>),
    Tuple(Vec<Expr>),
    List(Vec<Expr>),
    Literal(Literal),
    None,
}
