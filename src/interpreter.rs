use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt;
use std::mem::replace;
use std::rc::Rc;

use crate::{RuntimeError, RuntimeResult};
use crate::error::CodePos;
use crate::parse::expr::{Expr, ExprType};
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
    // Any, not used currently
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
            "type" => Ok(Self::DataType),
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
        }
    }
}

impl fmt::Debug for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum CoreFnType {
    DefVar,
    Set,
    If,
    Into,
    Ret,
    Println,
    Print,
    TypeOf,
    RetInput,
    Idx,
    Lambda,
    Fn,
    While,
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
    pub fn args_count(&self) -> (usize, bool) {
        match self {
            Self::If => (2, true),
            Self::Println => (0, true),
            Self::Print => (1, true),
            Self::Ret => (1, true),

            Self::DefVar => (2, false),
            Self::Set => (2, false),
            Self::Into => (2, false),
            Self::TypeOf => (1, false),
            Self::RetInput => (1, false),
            Self::Idx => (2, false),
            Self::Lambda => (2, false),
            Self::Fn => (3, false),
            Self::While => (2, false),
            // Operator functions
            Self::Neg => (1, false),
            Self::Not => (1, false),
            Self::Mul => (2, false),
            Self::Div => (2, false),
            Self::Mod => (2, false),
            Self::Add => (2, false),
            Self::Sub => (2, false),
            Self::ShiftLeft => (2, false),
            Self::ShiftRight => (2, false),
            Self::Less => (2, false),
            Self::Greater => (2, false),
            Self::LessEq => (2, false),
            Self::GreaterEq => (2, false),
            Self::Equals => (2, false),
            Self::NotEq => (2, false),
            Self::BitAnd => (2, false),
            Self::BitXor => (2, false),
            Self::BitOr => (2, false),
            Self::And => (2, false),
            Self::Or => (2, false),
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
            "into" => Ok(Self::Into),
            "ret" => Ok(Self::Ret),
            "println" => Ok(Self::Println),
            "print" => Ok(Self::Print),
            "typeof" => Ok(Self::TypeOf),
            "@" => Ok(Self::RetInput),
            "idx" => Ok(Self::Idx),
            "lam" => Ok(Self::Lambda),
            "fn" => Ok(Self::Fn),
            "while" => Ok(Self::While),
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
            "neq" => Ok(Self::NotEq),
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
pub enum Data<'a> {
    Int(i32),
    UInt(u32),
    Float(f32),
    Bool(bool),
    None,
    Str(String),
    Lambda {
        exprs: &'a [Expr],
        args: Vec<(String, Vec<DataType>)>,
        etc: bool,
        vars: HashMap<String, Vec<Rc<RefCell<Data<'a>>>>>,
        prop_ret: bool,
    },
    List(Vec<Data<'a>>),
    DataType(DataType),
    CoreFn(CoreFnType),
}

impl PartialEq for Data<'_> {
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
            Self::Lambda { .. } => false,
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

impl fmt::Display for Data<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::UInt(u) => write!(f, "{}", u),
            Self::Float(fl) => {
                if *fl == fl.floor() {
                    write!(f, "{}.0", fl)
                } else {
                    write!(f, "{}", fl)
                }
            }
            Self::Bool(b) => write!(f, "{}", b),
            Self::None => write!(f, "none"),
            Self::Str(s) => write!(f, "{}", s),
            Self::Lambda { .. } | Self::CoreFn(_) => write!(f, "lambda"),
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

impl Data<'_> {
    pub fn data_type(&self) -> DataType {
        match self {
            Self::Int(_) => DataType::Int,
            Self::UInt(_) => DataType::UInt,
            Self::Float(_) => DataType::Float,
            Self::Bool(_) => DataType::Bool,
            Self::None => DataType::None,
            Self::Str(_) => DataType::Str,
            Self::Lambda { .. } | Self::CoreFn(_) => DataType::Lambda,
            Self::List(_) => DataType::List,
            Self::DataType(_) => DataType::DataType,
        }
    }

    pub fn is(&self, types: &[DataType]) -> bool {
        let mut result = types.is_empty();
        for t in types.iter() {
            result = self.data_type() == *t;
            if result {
                break;
            }
        }
        result
    }
}

fn override_var<'a>(
    vars: &mut HashMap<String, Vec<Rc<RefCell<Data<'a>>>>>,
    name: &String,
    data: Rc<RefCell<Data<'a>>>,
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

fn call<'a>(
    fn_pos: CodePos,
    mut fn_name: Data<'a>,
    args: &'a [Expr],
    vars: &mut HashMap<String, Vec<Rc<RefCell<Data<'a>>>>>,
    scope_vars: &mut Vec<String>,
) -> RuntimeResult<(bool, Data<'a>)> {
    match &mut fn_name {
        Data::Lambda {
            exprs,
            args: lambda_args,
            etc,
            vars: lambda_vars,
            prop_ret,
        } => {
            let etc = *etc;
            // Arguments
            if lambda_args.len() != args.len() && !etc || (args.len() < lambda_args.len() && etc) {
                return Err(RuntimeError::MismatchedParameterCount(
                    fn_pos,
                    lambda_args.len(),
                    args.len(),
                    etc,
                ));
            }
            let mut lambda_scope_vars = Vec::new();
            for i in 0..lambda_args.len() {
                let arg = interpret(&args[i], vars, scope_vars)?;
                if arg.0 {
                    return Ok(arg);
                }
                if arg.1.is(&lambda_args[i].1) {
                    override_var(
                        lambda_vars,
                        &lambda_args[i].0,
                        Rc::new(RefCell::new(arg.1)),
                        &mut lambda_scope_vars,
                    )?;
                } else {
                    return Err(RuntimeError::MismatchedDataTypes(
                        args[i].0,
                        lambda_args[i].1.clone(),
                        arg.1.data_type(),
                    ));
                }
            }
            if etc {
                let mut etc_arg = Vec::with_capacity(args.len() - lambda_args.len());
                for i in lambda_args.len()..args.len() {
                    let arg = interpret(&args[i], vars, scope_vars)?;
                    if arg.0 {
                        return Ok(arg);
                    }
                    etc_arg.push(arg.1);
                }
                override_var(
                    lambda_vars,
                    &"etc".to_string(),
                    Rc::new(RefCell::new(Data::List(etc_arg))),
                    &mut lambda_scope_vars,
                )?;
            }
            let mut val = (false, Data::None);
            // Exprs
            for expr in exprs.iter() {
                val = interpret(expr, lambda_vars, &mut lambda_scope_vars)?;
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
            if etc {
                undo_var_override(vars, &"etc".to_string())?;
            }
            Ok((val.0 && *prop_ret, val.1))
        }
        Data::CoreFn(core_fn_type) => {
            let args_count = core_fn_type.args_count();
            if (args.len() != args_count.0 && !args_count.1)
                || (args.len() < args_count.0 && args_count.1)
            {
                return Err(RuntimeError::MismatchedParameterCount(
                    fn_pos,
                    args_count.0,
                    args.len(),
                    args_count.1,
                ));
            }
            let mut interpreted_args = Vec::new();
            for arg in args.iter() {
                let arg = interpret(arg, vars, scope_vars)?;
                if arg.0 {
                    return Ok(arg);
                }
                interpreted_args.push(arg.1);
            }

            match core_fn_type {
                CoreFnType::DefVar => {
                    let arg0_type = interpreted_args[0].data_type();
                    if let Data::Str(s) = replace(&mut interpreted_args[0], Data::None) {
                        if scope_vars.contains(&s) {
                            return Err(RuntimeError::RedefinedVariableInTheSameScope(
                                args[0].0, s,
                            ));
                        }
                        override_var(
                            vars,
                            &s,
                            Rc::new(RefCell::new(replace(&mut interpreted_args[1], Data::None))),
                            scope_vars,
                        )?;
                        Ok((false, Data::None))
                    } else {
                        Err(RuntimeError::MismatchedDataTypes(
                            args[0].0,
                            vec![DataType::Str],
                            arg0_type,
                        ))
                    }
                }
                CoreFnType::Set => {
                    let arg0_type = interpreted_args[0].data_type();
                    if let Data::Str(s) = replace(&mut interpreted_args[0], Data::None) {
                        if let Some(var) = vars.get_mut(&s) {
                            *var.last().unwrap().borrow_mut() =
                                replace(&mut interpreted_args[1], Data::None);
                            Ok((false, Data::None))
                        } else {
                            Err(RuntimeError::VarIsNotDefined(args[0].0, s))
                        }
                    } else {
                        Err(RuntimeError::MismatchedDataTypes(
                            args[0].0,
                            vec![DataType::Str],
                            arg0_type,
                        ))
                    }
                }
                CoreFnType::If => {
                    if let Data::Bool(b) = interpreted_args[0] {
                        if b {
                            if let Data::Lambda { prop_ret, .. } = &mut interpreted_args[1] {
                                *prop_ret = true;
                            } else if let Data::CoreFn(_) = &interpreted_args[1] {} else {
                                return Err(RuntimeError::MismatchedDataTypes(
                                    args[1].0,
                                    vec![DataType::Lambda],
                                    interpreted_args[1].data_type(),
                                ));
                            }

                            return call(
                                args[1].0,
                                replace(&mut interpreted_args[1], Data::None),
                                &[],
                                vars,
                                scope_vars,
                            );
                        }

                        let case_list = &mut interpreted_args[2..];
                        for i in 0..(case_list.len() / 2) {
                            let cond = call(
                                args[i * 2 + 2].0,
                                replace(&mut case_list[i * 2], Data::None),
                                &[],
                                vars,
                                scope_vars,
                            )?;
                            if cond.0 {
                                return Ok(cond);
                            }

                            if let Data::Bool(b) = cond.1 {
                                if b {
                                    if let Data::Lambda { prop_ret, .. } = &mut case_list[i * 2 + 1]
                                    {
                                        *prop_ret = true;
                                    } else if let Data::CoreFn(_) = &case_list[i * 2 + 1] {} else {
                                        return Err(RuntimeError::MismatchedDataTypes(
                                            args[i * 2 + 3].0,
                                            vec![DataType::Lambda],
                                            case_list[i * 2 + 1].data_type(),
                                        ));
                                    }

                                    return call(
                                        args[i * 2 + 3].0,
                                        replace(&mut case_list[i * 2 + 1], Data::None),
                                        &[],
                                        vars,
                                        scope_vars,
                                    );
                                }
                            } else {
                                return Err(RuntimeError::MismatchedReturnDataTypes(
                                    args[i * 2 + 2].0,
                                    vec![DataType::Bool],
                                    case_list[i * 2 + 1].data_type(),
                                ));
                            }
                        }
                        return if case_list.len() % 2 == 1 {
                            let else_case = case_list.last_mut().unwrap();
                            if let Data::Lambda { prop_ret, .. } = else_case {
                                *prop_ret = true;
                            } else if let Data::CoreFn(_) = else_case {} else {
                                return Err(RuntimeError::MismatchedDataTypes(
                                    args.last().unwrap().0,
                                    vec![DataType::Lambda],
                                    case_list[0].data_type(),
                                ));
                            }

                            call(
                                args.last().unwrap().0,
                                replace(else_case, Data::None),
                                &[],
                                vars,
                                scope_vars,
                            )
                        } else {
                            Ok((false, Data::None))
                        };
                    } else {
                        Err(RuntimeError::MismatchedDataTypes(
                            args[0].0,
                            vec![DataType::Bool],
                            interpreted_args[0].data_type(),
                        ))
                    }
                }
                CoreFnType::Into => {
                    let into_data_type = if let Data::DataType(data_type) = interpreted_args[0] {
                        data_type
                    } else {
                        return Err(RuntimeError::MismatchedDataTypes(
                            args[0].0,
                            vec![DataType::DataType],
                            interpreted_args[0].data_type(),
                        ));
                    };

                    let from = replace(&mut interpreted_args[1], Data::None);

                    match from {
                        Data::None => match into_data_type {
                            DataType::Bool => Ok((false, Data::Bool(false))),
                            DataType::None => Ok((false, Data::None)),
                            DataType::List => Ok((false, Data::List(vec![Data::None]))),
                            DataType::Float => Ok((false, Data::Float(0.0))),
                            DataType::Int => Ok((false, Data::Int(0))),
                            DataType::UInt => Ok((false, Data::UInt(0))),
                            DataType::Str => Ok((false, Data::Str("none".to_string()))),
                            _ => Err(RuntimeError::ConvertError(
                                args[1].0,
                                into_data_type,
                                DataType::None,
                            )),
                        },
                        Data::Bool(from) => match into_data_type {
                            DataType::Bool => Ok((false, Data::Bool(from))),
                            DataType::List => Ok((false, Data::List(vec![Data::Bool(from)]))),
                            DataType::Str => Ok((false, Data::Str(from.to_string()))),
                            DataType::UInt => Ok((false, Data::UInt(if from { 1 } else { 0 }))),
                            DataType::Int => Ok((false, Data::Int(if from { 1 } else { 0 }))),
                            DataType::Float => Ok((false, Data::Float(if from { 1. } else { 0. }))),
                            _ => Err(RuntimeError::ConvertError(
                                args[1].0,
                                into_data_type,
                                DataType::Bool,
                            )),
                        },
                        Data::Float(from) => match into_data_type {
                            DataType::Bool => Ok((false, Data::Bool(from != 0.))),
                            DataType::List => Ok((false, Data::List(vec![Data::Float(from)]))),
                            DataType::Str => Ok((false, Data::Str(from.to_string()))),
                            DataType::UInt => Ok((false, Data::UInt(from as u32))),
                            DataType::Int => Ok((false, Data::Int(from as i32))),
                            DataType::Float => Ok((false, Data::Float(from))),
                            _ => Err(RuntimeError::ConvertError(
                                args[1].0,
                                into_data_type,
                                DataType::Float,
                            )),
                        },
                        Data::Int(from) => match into_data_type {
                            DataType::Bool => Ok((false, Data::Bool(from != 0))),
                            DataType::List => Ok((false, Data::List(vec![Data::Int(from)]))),
                            DataType::Str => Ok((false, Data::Str(from.to_string()))),
                            DataType::UInt => Ok((false, Data::UInt(from as u32))),
                            DataType::Int => Ok((false, Data::Int(from))),
                            DataType::Float => Ok((false, Data::Float(from as f32))),
                            _ => Err(RuntimeError::ConvertError(
                                args[1].0,
                                into_data_type,
                                DataType::Int,
                            )),
                        },
                        Data::UInt(from) => match into_data_type {
                            DataType::Bool => Ok((false, Data::Bool(from != 0))),
                            DataType::List => Ok((false, Data::List(vec![Data::UInt(from)]))),
                            DataType::Str => Ok((false, Data::Str(from.to_string()))),
                            DataType::UInt => Ok((false, Data::UInt(from))),
                            DataType::Int => Ok((false, Data::Int(from as i32))),
                            DataType::Float => Ok((false, Data::Float(from as f32))),
                            _ => Err(RuntimeError::ConvertError(
                                args[1].0,
                                into_data_type,
                                DataType::UInt,
                            )),
                        },
                        Data::Str(from) => match into_data_type {
                            DataType::Bool => Ok((false, Data::Bool(from.parse().unwrap()))),
                            DataType::List => Ok((false, Data::List(vec![Data::Str(from)]))),
                            DataType::Str => Ok((false, Data::Str(from))),
                            DataType::UInt => Ok((false, Data::UInt(from.parse().unwrap()))),
                            DataType::Int => Ok((false, Data::Int(from.parse().unwrap()))),
                            DataType::Float => Ok((false, Data::Float(from.parse().unwrap()))),
                            _ => Err(RuntimeError::ConvertError(
                                args[1].0,
                                into_data_type,
                                DataType::Str,
                            )),
                        },
                        _ => Err(RuntimeError::ConvertError(
                            args[0].0,
                            into_data_type,
                            from.data_type(),
                        )),
                    }
                }
                CoreFnType::Ret => {
                    if interpreted_args.len() == 1 {
                        Ok((true, replace(&mut interpreted_args[0], Data::None)))
                    } else {
                        Ok((true, Data::List(interpreted_args)))
                    }
                }
                CoreFnType::Println | CoreFnType::Print => {
                    for (i, arg) in interpreted_args.iter().enumerate() {
                        if i == args.len() - 1 {
                            print!("{}", arg);
                        } else {
                            print!("{} ", arg);
                        }
                    }
                    if *core_fn_type == CoreFnType::Println {
                        println!();
                    }
                    Ok((false, Data::None))
                }
                CoreFnType::TypeOf => Ok((false, Data::DataType(interpreted_args[0].data_type()))),
                CoreFnType::RetInput => Ok((false, replace(&mut interpreted_args[0], Data::None))),
                CoreFnType::Idx => {
                    let arg0_type = interpreted_args[0].data_type();
                    match replace(&mut interpreted_args[0], Data::None) {
                        Data::List(mut list) => match interpreted_args[1] {
                            Data::UInt(u) => {
                                Ok((false, replace(&mut list[u as usize], Data::None)))
                            }
                            Data::Int(i) => {
                                let idx = if i < 0 {
                                    (i + list.len() as i32) as usize
                                } else {
                                    i as usize
                                };

                                Ok((false, replace(&mut list[idx], Data::None)))
                            }
                            _ => Err(RuntimeError::MismatchedDataTypes(
                                args[1].0,
                                vec![DataType::Int, DataType::UInt],
                                interpreted_args[1].data_type(),
                            )),
                        },
                        Data::Str(s) => match interpreted_args[1] {
                            Data::UInt(u) => Ok((
                                false,
                                Data::Str(s.chars().nth(u as usize).unwrap().to_string()),
                            )),
                            Data::Int(i) => {
                                let idx = if i < 0 {
                                    (i + s.len() as i32) as usize
                                } else {
                                    i as usize
                                };

                                Ok((
                                    false,
                                    Data::Str(s.chars().nth(idx as usize).unwrap().to_string()),
                                ))
                            }
                            _ => Err(RuntimeError::MismatchedDataTypes(
                                args[1].0,
                                vec![DataType::Int, DataType::UInt],
                                interpreted_args[1].data_type(),
                            )),
                        },
                        _ => Err(RuntimeError::MismatchedDataTypes(
                            args[0].0,
                            vec![DataType::List, DataType::Str],
                            arg0_type,
                        )),
                    }
                }
                CoreFnType::Lambda | CoreFnType::Fn => {
                    let base_idx = if *core_fn_type == CoreFnType::Fn {
                        1
                    } else {
                        0
                    };

                    let base_arg_type = interpreted_args[base_idx].data_type();
                    if let Data::List(mut list) =
                    replace(&mut interpreted_args[base_idx], Data::None)
                    {
                        if let Data::Lambda {
                            args: inputs,
                            prop_ret,
                            etc,
                            ..
                        } = &mut interpreted_args[base_idx + 1]
                        {
                            let mut list = &mut list[..];
                            if let Some(Data::Bool(prop_ret_val)) = list.last() {
                                *prop_ret = *prop_ret_val;
                                let list_len = list.len();
                                list = &mut list[..(list_len - 1)];
                            }
                            if let Some(Data::Str(s)) = list.last() {
                                if s == "etc" {
                                    *etc = true;
                                    let list_len = list.len();
                                    list = &mut list[..(list_len - 1)];
                                }
                            }
                            const ETC_ERR: &str = "\"etc\" is a reserved argument name and can be used only at the end of the arguments list.";
                            const STRUCTURE_ERR: &str = "The lambda and fn functions expect that the list argument will be `[ 'NAME TYPE TYPE2 ... ]`";
                            const TYPE_REPEAT_ERR: &str = "A lambda input type cannot be repeated because that doesn't make sense";
                            for el in list.iter_mut() {
                                match el {
                                    Data::Str(s) => {
                                        if s == "etc" {
                                            return Err(RuntimeError::Raised(
                                                fn_pos,
                                                ETC_ERR.to_string(),
                                            ));
                                        }
                                        inputs.push((replace(s, String::new()), Vec::new()));
                                    }
                                    Data::List(arg) => {
                                        if let Data::Str(s) = replace(&mut arg[0], Data::None) {
                                            if s == "etc" {
                                                return Err(RuntimeError::Raised(
                                                    fn_pos,
                                                    ETC_ERR.to_string(),
                                                ));
                                            }
                                            let mut types = Vec::new();
                                            for i in 1..arg.len() {
                                                if let Data::DataType(t) = arg[i] {
                                                    if types.contains(&t) {
                                                        return Err(RuntimeError::Raised(
                                                            fn_pos,
                                                            TYPE_REPEAT_ERR.to_string(),
                                                        ));
                                                    } else {
                                                        types.push(t);
                                                    }
                                                } else {
                                                    return Err(RuntimeError::Raised(
                                                        fn_pos,
                                                        STRUCTURE_ERR.to_string(),
                                                    ));
                                                }
                                            }
                                            inputs.push((s, types));
                                        } else {
                                            return Err(RuntimeError::Raised(
                                                fn_pos,
                                                STRUCTURE_ERR.to_string(),
                                            ));
                                        }
                                    }
                                    _ => {
                                        return Err(RuntimeError::Raised(fn_pos, "The lambda and fn functions expect that the argument will be either a string `'NAME` a list `[ 'NAME TYPE ]`".to_string()));
                                    }
                                }
                            }
                            if *core_fn_type == CoreFnType::Lambda {
                                Ok((
                                    false,
                                    replace(&mut interpreted_args[base_idx + 1], Data::None),
                                ))
                            } else {
                                let arg0_type = interpreted_args[0].data_type();
                                if let Data::Str(s) = replace(&mut interpreted_args[0], Data::None)
                                {
                                    if scope_vars.contains(&s) {
                                        return Err(RuntimeError::RedefinedVariableInTheSameScope(
                                            args[0].0, s,
                                        ));
                                    }
                                    override_var(
                                        vars,
                                        &s,
                                        Rc::new(RefCell::new(replace(
                                            &mut interpreted_args[base_idx + 1],
                                            Data::None,
                                        ))),
                                        scope_vars,
                                    )?;
                                    Ok((false, Data::None))
                                } else {
                                    Err(RuntimeError::MismatchedDataTypes(
                                        args[0].0,
                                        vec![DataType::Str],
                                        arg0_type,
                                    ))
                                }
                            }
                        } else {
                            Err(RuntimeError::MismatchedDataTypes(
                                args[base_idx + 1].0,
                                vec![DataType::Lambda],
                                interpreted_args[base_idx + 1].data_type(),
                            ))
                        }
                    } else {
                        Err(RuntimeError::MismatchedDataTypes(
                            args[base_idx].0,
                            vec![DataType::List],
                            base_arg_type,
                        ))
                    }
                }
                CoreFnType::While => loop {
                    let mut cond_fn = interpreted_args[0].clone();
                    if let Data::Lambda { prop_ret, .. } = &mut cond_fn {
                        *prop_ret = true;
                    }
                    let cond = call(args[0].0, cond_fn, &[], vars, scope_vars)?;
                    if cond.0 {
                        return Ok(cond);
                    }
                    if let Data::Bool(cond) = cond.1 {
                        if cond {
                            let mut res_fn = interpreted_args[1].clone();
                            if let Data::Lambda { prop_ret, .. } = &mut res_fn {
                                *prop_ret = true;
                            }
                            let res = call(args[1].0, res_fn, &[], vars, scope_vars)?;
                            if res.0 {
                                return Ok(res);
                            }
                        } else {
                            break Ok((false, Data::None));
                        }
                    }
                }
                // Operator functions
                _ => {
                    let arg = replace(&mut interpreted_args[0], Data::None);

                    match core_fn_type {
                        // Unary Operators
                        CoreFnType::Neg => match arg {
                            Data::Int(i) => Ok((false, Data::Int(-i))),
                            Data::Float(f) => Ok((false, Data::Float(-f))),
                            _ => Err(RuntimeError::MismatchedDataTypes(
                                args[0].0,
                                vec![DataType::Int, DataType::Float],
                                arg.data_type(),
                            )),
                        },
                        CoreFnType::Not => match arg {
                            Data::Bool(b) => Ok((false, Data::Bool(!b))),
                            _ => Err(RuntimeError::MismatchedDataTypes(
                                args[0].0,
                                vec![DataType::Bool],
                                arg.data_type(),
                            )),
                        },
                        // Binary operators
                        _ => {
                            let arg2 = replace(&mut interpreted_args[1], Data::None);

                            match core_fn_type {
                                CoreFnType::Mul => match arg {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2 {
                                            Ok((false, Data::UInt(u * u2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::UInt],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2 {
                                            Ok((false, Data::Int(i * i2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::Int],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    Data::Float(f) => {
                                        if let Data::Float(f2) = arg2 {
                                            Ok((false, Data::Float(f * f2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::Float],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    _ => Err(RuntimeError::MismatchedDataTypes(
                                        args[0].0,
                                        vec![DataType::Int, DataType::UInt, DataType::Float],
                                        arg.data_type(),
                                    )),
                                },
                                CoreFnType::Div => match arg {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2 {
                                            Ok((false, Data::UInt(u / u2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::UInt],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2 {
                                            Ok((false, Data::Int(i / i2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::Int],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    Data::Float(f) => {
                                        if let Data::Float(f2) = arg2 {
                                            Ok((false, Data::Float(f / f2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::Float],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    _ => Err(RuntimeError::MismatchedDataTypes(
                                        args[0].0,
                                        vec![DataType::Int, DataType::UInt, DataType::Float],
                                        arg.data_type(),
                                    )),
                                },
                                CoreFnType::Mod => match arg {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2 {
                                            Ok((false, Data::UInt(u % u2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::UInt],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2 {
                                            Ok((false, Data::Int(i - (i / i2) * i2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::Int],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    Data::Float(f) => {
                                        if let Data::Float(f2) = arg2 {
                                            Ok((false, Data::Float(f - (f / f2).floor() * f2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::Float],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    _ => Err(RuntimeError::MismatchedDataTypes(
                                        args[0].0,
                                        vec![DataType::Int, DataType::UInt, DataType::Float],
                                        arg.data_type(),
                                    )),
                                },
                                CoreFnType::Add => match arg {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2 {
                                            Ok((false, Data::UInt(u + u2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::UInt],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2 {
                                            Ok((false, Data::Int(i + i2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::Int],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    Data::Float(f) => {
                                        if let Data::Float(f2) = arg2 {
                                            Ok((false, Data::Float(f + f2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::Float],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    Data::List(mut list) => {
                                        if let Data::List(list2) = arg2 {
                                            list.extend(list2);
                                            Ok((false, Data::List(list)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::List],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    Data::Str(s) => {
                                        Ok((false, Data::Str(format!("{}{}", s, &arg2))))
                                    }
                                    _ => Err(RuntimeError::MismatchedDataTypes(
                                        args[0].0,
                                        vec![
                                            DataType::Int,
                                            DataType::UInt,
                                            DataType::Float,
                                            DataType::List,
                                            DataType::Str,
                                        ],
                                        arg.data_type(),
                                    )),
                                },
                                CoreFnType::Sub => match arg {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2 {
                                            Ok((false, Data::UInt(u - u2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::UInt],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2 {
                                            Ok((false, Data::Int(i - i2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::Int],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    Data::Float(f) => {
                                        if let Data::Float(f2) = arg2 {
                                            Ok((false, Data::Float(f - f2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::Float],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    _ => Err(RuntimeError::MismatchedDataTypes(
                                        args[0].0,
                                        vec![DataType::Int, DataType::UInt, DataType::Float],
                                        arg.data_type(),
                                    )),
                                },
                                CoreFnType::ShiftLeft => match arg {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2 {
                                            Ok((false, Data::UInt(u << u2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::UInt],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2 {
                                            Ok((false, Data::Int(i << i2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::Int],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    _ => Err(RuntimeError::MismatchedDataTypes(
                                        args[0].0,
                                        vec![DataType::Int, DataType::UInt],
                                        arg.data_type(),
                                    )),
                                },
                                CoreFnType::ShiftRight => match arg {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2 {
                                            Ok((false, Data::UInt(u >> u2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::UInt],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2 {
                                            Ok((false, Data::Int(i >> i2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::Int],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    _ => Err(RuntimeError::MismatchedDataTypes(
                                        args[0].0,
                                        vec![DataType::Int, DataType::UInt],
                                        arg.data_type(),
                                    )),
                                },
                                CoreFnType::Less
                                | CoreFnType::Greater
                                | CoreFnType::LessEq
                                | CoreFnType::GreaterEq => match arg {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2 {
                                            Ok((
                                                false,
                                                Data::Bool(match core_fn_type {
                                                    CoreFnType::Less => u < u2,
                                                    CoreFnType::Greater => u > u2,
                                                    CoreFnType::LessEq => u <= u2,
                                                    CoreFnType::GreaterEq => u >= u2,
                                                    _ => panic!("Shouldn't happen"),
                                                }),
                                            ))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::UInt],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2 {
                                            Ok((
                                                false,
                                                Data::Bool(match core_fn_type {
                                                    CoreFnType::Less => i < i2,
                                                    CoreFnType::Greater => i > i2,
                                                    CoreFnType::LessEq => i <= i2,
                                                    CoreFnType::GreaterEq => i >= i2,
                                                    _ => panic!("Shouldn't happen"),
                                                }),
                                            ))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::Int],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    Data::Float(f) => {
                                        if let Data::Float(f2) = arg2 {
                                            Ok((
                                                false,
                                                Data::Bool(match core_fn_type {
                                                    CoreFnType::Less => f < f2,
                                                    CoreFnType::Greater => f > f2,
                                                    CoreFnType::LessEq => f <= f2,
                                                    CoreFnType::GreaterEq => f >= f2,
                                                    _ => panic!("Shouldn't happen"),
                                                }),
                                            ))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::Float],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    _ => Err(RuntimeError::MismatchedDataTypes(
                                        args[0].0,
                                        vec![DataType::Int, DataType::UInt, DataType::Float],
                                        arg.data_type(),
                                    )),
                                },
                                CoreFnType::Equals => Ok((false, Data::Bool(arg == arg2))),
                                CoreFnType::NotEq => Ok((false, Data::Bool(arg != arg2))),
                                CoreFnType::BitAnd => match arg {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2 {
                                            Ok((false, Data::UInt(u & u2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::UInt],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2 {
                                            Ok((false, Data::Int(i & i2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::Int],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    _ => Err(RuntimeError::MismatchedDataTypes(
                                        args[0].0,
                                        vec![DataType::Int, DataType::UInt],
                                        arg.data_type(),
                                    )),
                                },
                                CoreFnType::BitXor => match arg {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2 {
                                            Ok((false, Data::UInt(u ^ u2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::UInt],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2 {
                                            Ok((false, Data::Int(i ^ i2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::Int],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    _ => Err(RuntimeError::MismatchedDataTypes(
                                        args[0].0,
                                        vec![DataType::Int, DataType::UInt],
                                        arg.data_type(),
                                    )),
                                },
                                CoreFnType::BitOr => match arg {
                                    Data::UInt(u) => {
                                        if let Data::UInt(u2) = arg2 {
                                            Ok((false, Data::UInt(u | u2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::UInt],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    Data::Int(i) => {
                                        if let Data::Int(i2) = arg2 {
                                            Ok((false, Data::Int(i | i2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::Int],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    _ => Err(RuntimeError::MismatchedDataTypes(
                                        args[0].0,
                                        vec![DataType::Int, DataType::UInt],
                                        arg.data_type(),
                                    )),
                                },
                                CoreFnType::And => match arg {
                                    Data::Bool(b) => {
                                        if let Data::Bool(b2) = arg2 {
                                            Ok((false, Data::Bool(b && b2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::Bool],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    _ => Err(RuntimeError::MismatchedDataTypes(
                                        args[0].0,
                                        vec![DataType::Bool],
                                        arg.data_type(),
                                    )),
                                },
                                CoreFnType::Or => match arg {
                                    Data::Bool(b) => {
                                        if let Data::Bool(b2) = arg2 {
                                            Ok((false, Data::Bool(b || b2)))
                                        } else {
                                            Err(RuntimeError::MismatchedDataTypes(
                                                args[1].0,
                                                vec![DataType::Bool],
                                                arg2.data_type(),
                                            ))
                                        }
                                    }
                                    _ => Err(RuntimeError::MismatchedDataTypes(
                                        args[0].0,
                                        vec![DataType::Bool],
                                        arg.data_type(),
                                    )),
                                },
                                _ => panic!("Shouldn't happen"),
                            }
                        }
                    }
                }
            }
        }
        Data::Str(_)
        | Data::Int(_)
        | Data::UInt(_)
        | Data::None
        | Data::Float(_)
        | Data::DataType(_)
        | Data::Bool(_)
        | Data::List(_) => {
            if args.len() == 0 {
                Ok((false, fn_name))
            } else {
                Err(RuntimeError::MismatchedParameterCount(
                    fn_pos,
                    0,
                    args.len(),
                    false,
                ))
            }
        }
    }
}

pub fn interpret<'a>(
    expr: &'a Expr,
    vars: &mut HashMap<String, Vec<Rc<RefCell<Data<'a>>>>>,
    scope_vars: &mut Vec<String>,
) -> RuntimeResult<(bool, Data<'a>)> {
    match &expr.1 {
        ExprType::List(exprs) => {
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
        ExprType::Lambda(lambda) => {
            let mut lambda_vars = HashMap::new();
            for (key, val) in vars.iter() {
                // automatically removes empty variable override stacks so the unwrap is safe.
                lambda_vars.insert(key.clone(), vec![val.last().unwrap().clone()]);
            }
            Ok((
                false,
                Data::Lambda {
                    exprs: lambda,
                    args: Vec::new(),
                    vars: lambda_vars,
                    prop_ret: false,
                    etc: false,
                },
            ))
        }
        ExprType::FnCall(fn_name, args) => {
            let fn_pos = (**fn_name).0;
            let fn_name = interpret(fn_name, vars, scope_vars)?;
            if fn_name.0 {
                Ok(fn_name)
            } else {
                call(fn_pos, fn_name.1, args, vars, scope_vars)
            }
        }
        ExprType::Var(name) => {
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
                        } else if let Ok(some) = CoreFnType::try_from(name.as_str()) {
                            Ok((false, Data::CoreFn(some)))
                        } else {
                            Err(RuntimeError::VarIsNotDefined(expr.0, name.clone()))
                        }
                    }
                }
            }
        }
        ExprType::Literal(literal) => match literal {
            Literal::Float(f) => Ok((false, Data::Float(*f))),
            Literal::Int(i) => Ok((false, Data::Int(*i))),
            Literal::UInt(u) => Ok((false, Data::UInt(*u))),
            Literal::Str(s) => Ok((false, Data::Str(s.clone()))),
        },
        ExprType::Scope(exprs) => {
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
