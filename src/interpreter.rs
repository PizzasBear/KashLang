use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::rc::Rc;

use crate::{RuntimeError, RuntimeResult};
use crate::parse::expr::Expr;
use crate::parse::expr::Literal;

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum DataType {
    Int,
    UInt,
    Float,
    Bool,
    None,
    Str,
    Lambda,
    List,
    DataType,
    Any,
}

impl<'a> TryFrom<&'a str> for DataType {
    type Error = ();
    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        match s {
            "int" => Ok(Self::Int),
            "uint" => Ok(Self::UInt),
            "float" => Ok(Self::Float),
            "bool" => Ok(Self::Bool),
            "str" => Ok(Self::Str),
            "lambda" => Ok(Self::Lambda),
            "list" => Ok(Self::List),
            "data_type" => Ok(Self::DataType),
            "any" => Ok(Self::Any),
            _ => Err(()),
        }
    }
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "Int"),
            Self::UInt => write!(f, "UInt"),
            Self::Float => write!(f, "Float"),
            Self::Bool => write!(f, "Bool"),
            Self::None => write!(f, "None"),
            Self::Str => write!(f, "Str"),
            Self::Lambda => write!(f, "Lambda"),
            Self::List => write!(f, "List"),
            Self::DataType => write!(f, "DataType"),
            Self::Any => write!(f, "Any"),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum CoreFnType {
    DefVar,
    Set,
    If,
    IfElse,
    Into,
    Ret,
    Print,
    TypeOf,
    RetInput,
    // Operator functions
    Neg,
    Not,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    ShiftLeft,
    ShiftRight,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    Equals,
    NotEq,
    BitAnd,
    BitXor,
    BitOr,
    And,
    Or,
}

impl CoreFnType {
    pub fn args_count(&self) -> usize {
        match self {
            Self::DefVar => 2,
            Self::Set => 2,
            Self::If => 2,
            Self::IfElse => 3,
            Self::Into => 2,
            Self::Ret => 1,
            Self::Print => 1,
            Self::TypeOf => 1,
            Self::RetInput => 1,
            // Operator functions
            Self::Neg => 1,
            Self::Not => 1,
            Self::Mul => 2,
            Self::Div => 2,
            Self::Mod => 2,
            Self::Add => 2,
            Self::Sub => 2,
            Self::ShiftLeft => 2,
            Self::ShiftRight => 2,
            Self::Less => 2,
            Self::Greater => 2,
            Self::LessEq => 2,
            Self::GreaterEq => 2,
            Self::Equals => 2,
            Self::NotEq => 2,
            Self::BitAnd => 2,
            Self::BitXor => 2,
            Self::BitOr => 2,
            Self::And => 2,
            Self::Or => 2,
        }
    }
}

impl<'a> TryFrom<&'a str> for CoreFnType {
    type Error = ();

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        match s {
            "let" => Ok(Self::DefVar),
            "set" => Ok(Self::Set),
            "if" => Ok(Self::If),
            "if_else" => Ok(Self::IfElse),
            "into" => Ok(Self::Into),
            "ret" => Ok(Self::Ret),
            "print" => Ok(Self::Print),
            "type_of" => Ok(Self::TypeOf),
            "@" => Ok(Self::RetInput),
            // Operator functions
            "neg" => Ok(Self::Neg),
            "not" => Ok(Self::Not),
            "mul" => Ok(Self::Mul),
            "div" => Ok(Self::Div),
            "mod" => Ok(Self::Mod),
            "add" => Ok(Self::Add),
            "sub" => Ok(Self::Sub),
            "shift_left" => Ok(Self::ShiftLeft),
            "shift_right" => Ok(Self::ShiftRight),
            "less" => Ok(Self::Less),
            "greater" => Ok(Self::Greater),
            "less_eq" => Ok(Self::LessEq),
            "greater_eq" => Ok(Self::GreaterEq),
            "eq" => Ok(Self::Equals),
            "diff" => Ok(Self::NotEq),
            "bit_and" => Ok(Self::BitAnd),
            "bit_xor" => Ok(Self::BitXor),
            "bit_or" => Ok(Self::BitOr),
            "and" => Ok(Self::And),
            "or" => Ok(Self::Or),
            _ => Err(()),
        }
    }
}

#[derive(Clone)]
pub enum Data {
    Int(i32),
    UInt(u32),
    Float(f32),
    Bool(bool),
    None,
    Str(String),
    Lambda(
        Vec<Expr>,
        Vec<(String, DataType)>,
        HashMap<String, Vec<Rc<RefCell<Data>>>>,
    ),
    List(Vec<Data>),
    DataType(DataType),
    CoreFn(CoreFnType),
}

impl PartialEq for Data {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Int(i) => {
                if let Self::Int(i_other) = other {
                    i == i_other
                } else {
                    false
                }
            }
            Self::UInt(u) => {
                if let Self::UInt(u_other) = other {
                    u == u_other
                } else {
                    false
                }
            }
            Self::Float(f) => {
                if let Self::Float(f_other) = other {
                    f == f_other
                } else {
                    false
                }
            }
            Self::Bool(b) => {
                if let Self::Bool(b_other) = other {
                    b == b_other
                } else {
                    false
                }
            }
            Self::None => {
                if let Self::None = other {
                    true
                } else {
                    false
                }
            }
            Self::Str(s) => {
                if let Self::Str(s_other) = other {
                    s == s_other
                } else {
                    false
                }
            }
            Self::Lambda(_, _, _) => false,
            Self::List(l) => {
                if let Self::List(l_other) = other {
                    if l.len() == l_other.len() {
                        for (el, el_other) in l.iter().zip(l_other.iter()) {
                            if el != el_other {
                                return false;
                            }
                        }
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            Self::DataType(dt) => {
                if let Self::DataType(dt_other) = other {
                    dt == dt_other
                } else {
                    false
                }
            }
            Self::CoreFn(_) => false,
        }
    }
}

impl fmt::Display for Data {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::UInt(u) => write!(f, "{}", u),
            Self::Float(fl) => write!(f, "{}", fl),
            Self::Bool(b) => write!(f, "{}", b),
            Self::None => write!(f, "none"),
            Self::Str(s) => write!(f, "{}", s),
            Self::Lambda(_, _, _) | Self::CoreFn(_) => write!(f, "lambda"),
            Self::List(ls) => match ls.len() {
                0 => write!(f, "[]"),
                1 => write!(f, "[ {} ]", ls[0]),
                _ => {
                    write!(f, "[ ")?;
                    for el in ls[..(ls.len() - 1)].iter() {
                        write!(f, "{}, ", el)?;
                    }
                    write!(f, "{} ]", ls.last().unwrap())
                }
            },
            Self::DataType(data_type) => write!(f, "{}", data_type),
        }
    }
}

impl Data {
    pub fn data_type(&self) -> DataType {
        match self {
            Self::Int(_) => DataType::Int,
            Self::UInt(_) => DataType::UInt,
            Self::Float(_) => DataType::Float,
            Self::Bool(_) => DataType::Bool,
            Self::None => DataType::None,
            Self::Str(_) => DataType::Str,
            Self::Lambda(_, _, _) | Self::CoreFn(_) => DataType::Lambda,
            Self::List(_) => DataType::List,
            Self::DataType(_) => DataType::DataType,
        }
    }
}

fn override_var(
    vars: &mut HashMap<String, Vec<Rc<RefCell<Data>>>>,
    name: &String,
    data: Rc<RefCell<Data>>,
    scope_vars: &mut Vec<String>,
) -> RuntimeResult<()> {
    if let Some(var_override_stack) = vars.get_mut(name) {
        var_override_stack.push(data);
    } else {
        vars.insert(name.clone(), vec![data]);
    }
    scope_vars.push(name.clone());
    Ok(())
}

fn undo_var_override(
    vars: &mut HashMap<String, Vec<Rc<RefCell<Data>>>>,
    name: &String,
) -> RuntimeResult<()> {
    let mut remove_var = false;
    if let Some(var_override_stack) = vars.get_mut(name) {
        var_override_stack.pop();
        remove_var = var_override_stack.is_empty();
    }
    if remove_var {
        vars.remove(name);
    }

    Ok(())
}

fn call(
    fn_name: &Expr,
    args: &[Expr],
    vars: &mut HashMap<String, Vec<Rc<RefCell<Data>>>>,
    scope_vars: &mut Vec<String>,
) -> RuntimeResult<(bool, Data)> {
    let fn_name = interpret(fn_name, vars, scope_vars)?;
    if fn_name.0 {
        return Ok(fn_name);
    }
    match fn_name.1 {
        Data::Lambda(exprs, lambda_args, mut lambda_vars) => {
            // Arguments
            if lambda_args.len() != args.len() {
                return Err(RuntimeError::MismatchedParameterCount(
                    lambda_args.len(),
                    args.len(),
                ));
            }
            let mut lambda_scope_vars = Vec::new();
            for i in 0..lambda_args.len() {
                let arg = interpret(&args[i], vars, scope_vars)?;
                if arg.0 {
                    return Ok(arg);
                }
                if arg.1.data_type() == lambda_args[i].1
                    || lambda_args[i].1 == DataType::Any
                {
                    override_var(
                        &mut lambda_vars,
                        &lambda_args[i].0,
                        Rc::new(RefCell::new(arg.1)),
                        &mut lambda_scope_vars,
                    )?;
                } else {
                    return Err(RuntimeError::MismatchedDataTypes(
                        lambda_args[i].1,
                        arg.1.data_type(),
                    ));
                }
            }
            let mut val = (false, Data::None);
            // Exprs
            for expr in exprs.iter() {
                val =
                    interpret(expr, &mut lambda_vars, &mut lambda_scope_vars)?;
                if val.0 {
                    break;
                }
            }
            // Clear variables
            for scope_var in lambda_scope_vars.iter() {
                undo_var_override(vars, scope_var)?;
            }
            for i in 0..lambda_args.len() {
                undo_var_override(vars, &lambda_args[i].0)?;
            }
            Ok((false, val.1))
        }
        Data::List(list) => {
            if args.len() == 1 {
                let mut arg = interpret(&args[0], vars, scope_vars)?;
                if arg.0 {
                    return Ok(arg);
                }
                match &mut arg.1 {
                    Data::Lambda(_, inputs, _) => {
                        for el in list.iter() {
                            match el {
                                Data::Str(s) => {
                                    inputs.push((s.clone(), DataType::Any))
                                }
                                Data::List(arg) => {
                                    if let [Data::Str(s), Data::DataType(data_type)] =
                                    &arg[..]
                                    {
                                        inputs.push((s.clone(), *data_type));
                                    } else {
                                        return Err(
                                            RuntimeError::IncorrectListLambdaSyntax,
                                        );
                                    }
                                }
                                _ => {
                                    return Err(
                                        RuntimeError::IncorrectListLambdaSyntax,
                                    );
                                }
                            }
                        }
                        Ok(arg)
                    }
                    Data::UInt(u) => Ok((false, list[*u as usize].clone())),
                    Data::Int(i) => {
                        let idx = if *i < 0 {
                            (*i + list.len() as i32) as usize
                        } else {
                            *i as usize
                        };

                        Ok((false, list[idx].clone()))
                    }
                    _ => Err(RuntimeError::CallListNotRight),
                }
            } else {
                return Err(RuntimeError::CallListNotRight);
            }
        }
        Data::CoreFn(core_fn_type) => {
            let args_count = core_fn_type.args_count();
            if args_count != args.len() {
                return Err(RuntimeError::MismatchedParameterCount(
                    args.len(),
                    args_count,
                ));
            }
            match core_fn_type {
                CoreFnType::DefVar => {
                    let name = interpret(&args[0], vars, scope_vars)?;
                    if name.0 {
                        return Ok(name);
                    }
                    if let Data::Str(s) = name.1 {
                        let val = interpret(&args[1], vars, scope_vars)?;
                        if val.0 {
                            return Ok(val);
                        }
                        override_var(
                            vars,
                            &s,
                            Rc::new(RefCell::new(val.1)),
                            scope_vars,
                        )?;
                        Ok((false, Data::None))
                    } else {
                        Err(RuntimeError::MismatchedDataTypes(
                            DataType::Str,
                            name.1.data_type(),
                        ))
                    }
                }
                CoreFnType::Set => {
                    let name = interpret(&args[0], vars, scope_vars)?;
                    if name.0 {
                        return Ok(name);
                    }
                    if let Data::Str(s) = name.1 {
                        let val = interpret(&args[1], vars, scope_vars)?;
                        if val.0 {
                            return Ok(val);
                        }
                        if let Some(var) = vars.get_mut(&s) {
                            *(var.last().unwrap().borrow_mut()) = val.1;
                            Ok((false, Data::None))
                        } else {
                            Err(RuntimeError::VarIsNotDefined(s))
                        }
                    } else {
                        Err(RuntimeError::MismatchedDataTypes(
                            DataType::Str,
                            name.1.data_type(),
                        ))
                    }
                }
                CoreFnType::If | CoreFnType::IfElse => {
                    let arg = interpret(&args[0], vars, scope_vars)?;
                    if arg.0 {
                        return Ok(arg);
                    }
                    if let Data::Bool(b) = arg.1 {
                        if b {
                            call(&args[1], &[], vars, scope_vars)
                        } else if CoreFnType::IfElse == core_fn_type {
                            call(&args[2], &[], vars, scope_vars)
                        } else {
                            Ok((false, Data::None))
                        }
                    } else {
                        Err(RuntimeError::MismatchedDataTypes(
                            DataType::Bool,
                            arg.1.data_type(),
                        ))
                    }
                }
                CoreFnType::Into => {
                    let into_data_type = {
                        let arg = interpret(&args[0], vars, scope_vars)?;
                        if arg.0 {
                            return Ok(arg);
                        }
                        if let Data::DataType(data_type) = arg.1 {
                            data_type
                        } else {
                            return Err(RuntimeError::MismatchedDataTypes(
                                DataType::DataType,
                                arg.1.data_type(),
                            ));
                        }
                    };
                    let from = interpret(&args[1], vars, scope_vars)?;
                    if from.0 {
                        return Ok(from);
                    }
                    match from.1 {
                        Data::None => match into_data_type {
                            DataType::Bool => Ok((false, Data::Bool(false))),
                            DataType::None => Ok((false, Data::None)),
                            DataType::List => {
                                Ok((false, Data::List(vec![from.1])))
                            }
                            DataType::Float => Ok((false, Data::Float(0.0))),
                            DataType::Int => Ok((false, Data::Int(0))),
                            DataType::UInt => Ok((false, Data::UInt(0))),
                            DataType::Str => {
                                Ok((false, Data::Str("none".to_string())))
                            }
                            _ => Err(RuntimeError::ConvertError(
                                into_data_type,
                                DataType::None,
                            )),
                        },
                        Data::Bool(from) => match into_data_type {
                            DataType::Bool => Ok((false, Data::Bool(from))),
                            DataType::List => {
                                Ok((false, Data::List(vec![Data::Bool(from)])))
                            }
                            DataType::Str => {
                                Ok((false, Data::Str(from.to_string())))
                            }
                            DataType::UInt => Ok((
                                false,
                                Data::UInt(if from { 1 } else { 0 }),
                            )),
                            DataType::Int => {
                                Ok((false, Data::Int(if from { 1 } else { 0 })))
                            }
                            DataType::Float => Ok((
                                false,
                                Data::Float(if from { 1. } else { 0. }),
                            )),
                            _ => Err(RuntimeError::ConvertError(
                                into_data_type,
                                DataType::Bool,
                            )),
                        },
                        Data::Float(from) => match into_data_type {
                            DataType::Bool => {
                                Ok((false, Data::Bool(from != 0.)))
                            }
                            DataType::List => {
                                Ok((false, Data::List(vec![Data::Float(from)])))
                            }
                            DataType::Str => {
                                Ok((false, Data::Str(from.to_string())))
                            }
                            DataType::UInt => {
                                Ok((false, Data::UInt(from as u32)))
                            }
                            DataType::Int => {
                                Ok((false, Data::Int(from as i32)))
                            }
                            DataType::Float => Ok((false, Data::Float(from))),
                            _ => Err(RuntimeError::ConvertError(
                                into_data_type,
                                DataType::Float,
                            )),
                        },
                        Data::Int(from) => match into_data_type {
                            DataType::Bool => {
                                Ok((false, Data::Bool(from != 0)))
                            }
                            DataType::List => {
                                Ok((false, Data::List(vec![Data::Int(from)])))
                            }
                            DataType::Str => {
                                Ok((false, Data::Str(from.to_string())))
                            }
                            DataType::UInt => {
                                Ok((false, Data::UInt(from as u32)))
                            }
                            DataType::Int => Ok((false, Data::Int(from))),
                            DataType::Float => {
                                Ok((false, Data::Float(from as f32)))
                            }
                            _ => Err(RuntimeError::ConvertError(
                                into_data_type,
                                DataType::Int,
                            )),
                        },
                        Data::UInt(from) => match into_data_type {
                            DataType::Bool => {
                                Ok((false, Data::Bool(from != 0)))
                            }
                            DataType::List => {
                                Ok((false, Data::List(vec![Data::UInt(from)])))
                            }
                            DataType::Str => {
                                Ok((false, Data::Str(from.to_string())))
                            }
                            DataType::UInt => Ok((false, Data::UInt(from))),
                            DataType::Int => {
                                Ok((false, Data::Int(from as i32)))
                            }
                            DataType::Float => {
                                Ok((false, Data::Float(from as f32)))
                            }
                            _ => Err(RuntimeError::ConvertError(
                                into_data_type,
                                DataType::UInt,
                            )),
                        },
                        Data::Str(from) => match into_data_type {
                            DataType::Bool => {
                                Ok((false, Data::Bool(from.parse().unwrap())))
                            }
                            DataType::List => {
                                Ok((false, Data::List(vec![Data::Str(from)])))
                            }
                            DataType::Str => Ok((false, Data::Str(from))),
                            DataType::UInt => {
                                Ok((false, Data::UInt(from.parse().unwrap())))
                            }
                            DataType::Int => {
                                Ok((false, Data::Int(from.parse().unwrap())))
                            }
                            DataType::Float => {
                                Ok((false, Data::Float(from.parse().unwrap())))
                            }
                            _ => Err(RuntimeError::ConvertError(
                                into_data_type,
                                DataType::Str,
                            )),
                        },
                        _ => Err(RuntimeError::ConvertError(
                            DataType::Any,
                            from.1.data_type(),
                        )),
                    }
                }
                CoreFnType::Ret => {
                    let val = interpret(&args[0], vars, scope_vars)?;
                    Ok((true, val.1))
                }
                CoreFnType::Print => {
                    let val = interpret(&args[0], vars, scope_vars)?;
                    if val.0 {
                        return Ok(val);
                    }
                    println!("{}", val.1);
                    Ok((false, Data::None))
                }
                CoreFnType::TypeOf => {
                    let val = interpret(&args[0], vars, scope_vars)?;
                    if val.0 {
                        return Ok(val);
                    }
                    Ok((false, Data::DataType(val.1.data_type())))
                }
                CoreFnType::RetInput => interpret(&args[0], vars, scope_vars),
                // Operator functions
                _ => {
                    let arg = interpret(&args[0], vars, scope_vars)?;
                    if arg.0 {
                        return Ok(arg);
                    }
                    match core_fn_type {
                        // Unary Operators
                        CoreFnType::Neg => match arg.1 {
                            Data::Int(i) => Ok((false, Data::Int(-i))),
                            Data::Float(f) => Ok((false, Data::Float(-f))),
                            _ => Err(RuntimeError::ExpectedSignedNumber(
                                arg.1.data_type(),
                            )),
                        },
                        CoreFnType::Not => match arg.1 {
                            Data::Bool(b) => Ok((false, Data::Bool(!b))),
                            _ => Err(RuntimeError::MismatchedDataTypes(
                                DataType::Bool,
                                arg.1.data_type(),
                            )),
                        },
                        // Binary operators
                        _ => {
                            let arg2 = interpret(&args[1], vars, scope_vars)?;
                            if arg2.0 {
                                return Ok(arg2);
                            }
                            match core_fn_type {
                                CoreFnType::Mul => match arg.1 {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2.1 {
                                            Ok((false, Data::UInt(u * u2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::UInt, arg2.1.data_type()))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2.1 {
                                            Ok((false, Data::Int(i * i2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::Int, arg2.1.data_type()))
                                        }
                                    }
                                    Data::Float(f) => {
                                        if let Data::Float(f2) = arg2.1 {
                                            Ok((false, Data::Float(f * f2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::Float, arg2.1.data_type()))
                                        }
                                    }
                                    _ => Err(RuntimeError::ExpectedNumber(
                                        arg.1.data_type(),
                                    )),
                                },
                                CoreFnType::Div => match arg.1 {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2.1 {
                                            Ok((false, Data::UInt(u / u2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::UInt, arg2.1.data_type()))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2.1 {
                                            Ok((false, Data::Int(i / i2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::Int, arg2.1.data_type()))
                                        }
                                    }
                                    Data::Float(f) => {
                                        if let Data::Float(f2) = arg2.1 {
                                            Ok((false, Data::Float(f / f2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::Float, arg2.1.data_type()))
                                        }
                                    }
                                    _ => Err(RuntimeError::ExpectedNumber(
                                        arg.1.data_type(),
                                    )),
                                },
                                CoreFnType::Mod => match arg.1 {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2.1 {
                                            Ok((false, Data::UInt(u % u2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::UInt, arg2.1.data_type()))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2.1 {
                                            Ok((
                                                false,
                                                Data::Int(i - (i / i2) * i2),
                                            ))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::Int, arg2.1.data_type()))
                                        }
                                    }
                                    Data::Float(f) => {
                                        if let Data::Float(f2) = arg2.1 {
                                            Ok((
                                                false,
                                                Data::Float(
                                                    f - (f / f2).floor() * f2,
                                                ),
                                            ))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::Float, arg2.1.data_type()))
                                        }
                                    }
                                    _ => Err(RuntimeError::ExpectedNumber(
                                        arg.1.data_type(),
                                    )),
                                },
                                CoreFnType::Add => match arg.1 {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2.1 {
                                            Ok((false, Data::UInt(u + u2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::UInt, arg2.1.data_type()))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2.1 {
                                            Ok((false, Data::Int(i + i2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::Int, arg2.1.data_type()))
                                        }
                                    }
                                    Data::Float(f) => {
                                        if let Data::Float(f2) = arg2.1 {
                                            Ok((false, Data::Float(f + f2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::Float, arg2.1.data_type()))
                                        }
                                    }
                                    _ => Err(RuntimeError::ExpectedNumber(
                                        arg.1.data_type(),
                                    )),
                                },
                                CoreFnType::Sub => match arg.1 {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2.1 {
                                            Ok((false, Data::UInt(u - u2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::UInt, arg2.1.data_type()))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2.1 {
                                            Ok((false, Data::Int(i - i2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::Int, arg2.1.data_type()))
                                        }
                                    }
                                    Data::Float(f) => {
                                        if let Data::Float(f2) = arg2.1 {
                                            Ok((false, Data::Float(f - f2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::Float, arg2.1.data_type()))
                                        }
                                    }
                                    _ => Err(RuntimeError::ExpectedNumber(
                                        arg.1.data_type(),
                                    )),
                                },
                                CoreFnType::ShiftLeft => match arg.1 {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2.1 {
                                            Ok((false, Data::UInt(u << u2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::UInt, arg2.1.data_type()))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2.1 {
                                            Ok((false, Data::Int(i << i2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::Int, arg2.1.data_type()))
                                        }
                                    }
                                    _ => Err(RuntimeError::ExpectedInteger(
                                        arg.1.data_type(),
                                    )),
                                },
                                CoreFnType::ShiftRight => match arg.1 {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2.1 {
                                            Ok((false, Data::UInt(u >> u2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::UInt, arg2.1.data_type()))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2.1 {
                                            Ok((false, Data::Int(i >> i2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::Int, arg2.1.data_type()))
                                        }
                                    }
                                    _ => Err(RuntimeError::ExpectedInteger(
                                        arg.1.data_type(),
                                    )),
                                },
                                CoreFnType::Less
                                | CoreFnType::Greater
                                | CoreFnType::LessEq
                                | CoreFnType::GreaterEq => match arg.1 {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2.1 {
                                            Ok((false, Data::Bool(match core_fn_type {
                                                CoreFnType::Less => u < u2,
                                                CoreFnType::Greater => u > u2,
                                                CoreFnType::LessEq => u <= u2,
                                                CoreFnType::GreaterEq => u >= u2,
                                                _ => panic!("Shouldn't happen")
                                            })))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::UInt, arg2.1.data_type()))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2.1 {
                                            Ok((false, Data::Bool(match core_fn_type {
                                                CoreFnType::Less => i < i2,
                                                CoreFnType::Greater => i > i2,
                                                CoreFnType::LessEq => i <= i2,
                                                CoreFnType::GreaterEq => i >= i2,
                                                _ => panic!("Shouldn't happen")
                                            })))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::Int, arg2.1.data_type()))
                                        }
                                    }
                                    Data::Float(f) => {
                                        if let Data::Float(f2) = arg2.1 {
                                            Ok((false, Data::Bool(match core_fn_type {
                                                CoreFnType::Less => f < f2,
                                                CoreFnType::Greater => f > f2,
                                                CoreFnType::LessEq => f <= f2,
                                                CoreFnType::GreaterEq => f >= f2,
                                                _ => panic!("Shouldn't happen")
                                            })))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::Float, arg2.1.data_type()))
                                        }
                                    }
                                    _ => Err(RuntimeError::ExpectedNumber(
                                        arg.1.data_type(),
                                    )),
                                },
                                CoreFnType::Equals => {
                                    Ok((false, Data::Bool(arg.1 == arg2.1)))
                                }
                                CoreFnType::NotEq => {
                                    Ok((false, Data::Bool(arg.1 != arg2.1)))
                                }
                                CoreFnType::BitAnd => match arg.1 {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2.1 {
                                            Ok((false, Data::UInt(u & u2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::UInt, arg2.1.data_type()))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2.1 {
                                            Ok((false, Data::Int(i & i2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::Int, arg2.1.data_type()))
                                        }
                                    }
                                    _ => Err(RuntimeError::ExpectedInteger(
                                        arg.1.data_type(),
                                    )),
                                },
                                CoreFnType::BitXor => match arg.1 {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2.1 {
                                            Ok((false, Data::UInt(u ^ u2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::UInt, arg2.1.data_type()))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2.1 {
                                            Ok((false, Data::Int(i ^ i2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::Int, arg2.1.data_type()))
                                        }
                                    }
                                    _ => Err(RuntimeError::ExpectedInteger(
                                        arg.1.data_type(),
                                    )),
                                },
                                CoreFnType::BitOr => match arg.1 {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2.1 {
                                            Ok((false, Data::UInt(u | u2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::UInt, arg2.1.data_type()))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2.1 {
                                            Ok((false, Data::Int(i | i2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(DataType::Int, arg2.1.data_type()))
                                        }
                                    }
                                    _ => Err(RuntimeError::ExpectedInteger(
                                        arg.1.data_type(),
                                    )),
                                },
                                CoreFnType::And => match arg.1 {
                                    Data::Bool(b) => {
                                        if let Data::Bool(b2) = arg2.1 {
                                            Ok((false, Data::Bool(b && b2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                DataType::Bool,
                                                arg2.1.data_type(),
                                            ))
                                        }
                                    }
                                    _ => {
                                        Err(RuntimeError::MismatchedDataTypes(
                                            DataType::Bool,
                                            arg.1.data_type(),
                                        ))
                                    }
                                },
                                CoreFnType::Or => match arg.1 {
                                    Data::Bool(b) => {
                                        if let Data::Bool(b2) = arg2.1 {
                                            Ok((false, Data::Bool(b || b2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                DataType::Bool,
                                                arg2.1.data_type(),
                                            ))
                                        }
                                    }
                                    _ => {
                                        Err(RuntimeError::MismatchedDataTypes(
                                            DataType::Bool,
                                            arg.1.data_type(),
                                        ))
                                    }
                                },
                                _ => panic!("Shouldn't happen"),
                            }
                        }
                    }
                }
            }
        }
        _ => Err(RuntimeError::NotCallableDataType(fn_name.1.data_type())),
    }
}

pub fn interpret(
    expr: &Expr,
    vars: &mut HashMap<String, Vec<Rc<RefCell<Data>>>>,
    scope_vars: &mut Vec<String>,
) -> RuntimeResult<(bool, Data)> {
    match expr {
        Expr::List(exprs) => {
            let mut list = Vec::with_capacity(exprs.len());
            for expr in exprs.iter() {
                let val = interpret(expr, vars, scope_vars)?;
                if val.0 {
                    return Ok(val);
                }
                list.push(val.1);
            }

            Ok((false, Data::List(list)))
        }
        Expr::Lambda(lambda) => {
            let mut lambda_vars = HashMap::new();
            for (key, val) in vars.iter() {
                // automatically removes empty variable override stacks so the unwrap is safe.
                lambda_vars
                    .insert(key.clone(), vec![val.last().unwrap().clone()]);
            }
            Ok((false, Data::Lambda(lambda.clone(), Vec::new(), lambda_vars)))
        }
        Expr::FnCall(fn_name, args) => call(fn_name, args, vars, scope_vars),
        Expr::Var(name) => {
            if let Some(some) = vars.get(name) {
                Ok((false, some.last().unwrap().borrow().clone()))
            } else {
                match name.as_str() {
                    "false" => Ok((false, Data::Bool(false))),
                    "true" => Ok((false, Data::Bool(true))),
                    "none" => Ok((false, Data::None)),
                    _ => {
                        if let Ok(some) = DataType::try_from(name.as_str()) {
                            Ok((false, Data::DataType(some)))
                        } else if let Ok(some) =
                        CoreFnType::try_from(name.as_str())
                        {
                            Ok((false, Data::CoreFn(some)))
                        } else {
                            Err(RuntimeError::VarIsNotDefined(name.clone()))
                        }
                    }
                }
            }
        }
        Expr::Literal(literal) => match literal {
            Literal::Float(f) => Ok((false, Data::Float(*f))),
            Literal::Int(i) => Ok((false, Data::Int(*i))),
            Literal::UInt(u) => Ok((false, Data::UInt(*u))),
            Literal::Str(s) => Ok((false, Data::Str(s.clone()))),
        },
        Expr::Scope(exprs) => {
            let mut scope_vars = Vec::new();
            let mut val = (false, Data::None);
            // Exprs
            for expr in exprs.iter() {
                val = interpret(expr, vars, &mut scope_vars)?;
                if val.0 {
                    break;
                }
            }
            // Clear variables
            for scope_var in scope_vars.iter() {
                undo_var_override(vars, scope_var)?;
            }
            Ok(val)
        }
    }
}
