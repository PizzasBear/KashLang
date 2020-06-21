use std::convert::TryInto;
use std::sync::Arc;

use futures::future::{BoxFuture, FutureExt};
use tokio::task::{JoinHandle, spawn};

use expr::Expr;

use crate::{CompileError, CompileResult, lexer::Token};
use crate::error::CodePos;

pub mod expr;

pub fn parse_token(
    token_idx: &mut usize,
    arc_tokens: Arc<[(Token, Option<CodePos>)]>,
    range: std::ops::Range<usize>,
) -> CompileResult<Option<JoinHandle<CompileResult<Expr>>>> {
    let tokens = &arc_tokens[range.clone()];
    match &tokens[*token_idx].0 {
        Token::OpenBlock {
            close_idx,
            block_level,
            ..
        } => {
            let tmp = *token_idx + range.start;
            *token_idx = *close_idx - range.start;
            Ok(Some(spawn(parse(
                tmp..(*close_idx + 1),
                arc_tokens.clone(),
                *block_level,
            ))))
        }
        Token::CloseBlock { .. } => {
            panic!("Close block should be skipped.");
        }
        Token::Literal(literal) => {
            let literal = literal.clone();
            Ok(Some(spawn(async { Ok(Expr::Literal(literal)) })))
        }
        Token::Id(id) => {
            let id = id.clone();
            Ok(Some(spawn(async { Ok(Expr::Var(id)) })))
        }
        Token::Operator(op) => {
            if let Token::OpenBlock {
                ch,
                close_idx,
                block_level,
            } = tokens[*token_idx + 1].0
            {
                let tmp = *token_idx + range.start;
                *token_idx = close_idx - range.start;
                if (ch == '(' || ch == '{') && op == "$" {
                    return Ok(Some(spawn(parse(
                        tmp..(close_idx + 1),
                        arc_tokens.clone(),
                        block_level,
                    ))));
                }
            }
            return Err(CompileError::UnexpectedOperator(
                tokens[*token_idx].1,
                op.clone(),
            ));
        }
        Token::NewLine => Ok(None),
    }
}

pub fn parse_math(
    range: std::ops::Range<usize>,
    arc_tokens: Arc<[(Token, Option<CodePos>)]>,
    _block_level: usize,
) -> BoxFuture<'static, CompileResult<Expr>> {
    async move {
        let tokens = &arc_tokens[range.clone()];

        let mut i = 0;
        let mut op = None::<(expr::Operator, usize)>;
        let mut is_binary = false;
        while i < tokens.len() {
            match &tokens[i].0 {
                Token::Operator(tmp_op_str) if tmp_op_str != "$" => {
                    let tmp_op: expr::Operator =
                        (tokens[i].1, tmp_op_str, is_binary).try_into()?;
                    if let Some(op_unwrapped) = op {
                        if op_unwrapped.0.order() < tmp_op.order() {
                            op = Some((tmp_op, i));
                        }
                    } else {
                        op = Some((tmp_op, i));
                    }
                    is_binary = false;
                }
                _ => {
                    if is_binary {
                        return Err(CompileError::ExpectedBinaryOperator(
                            tokens[i].1.clone(),
                        ));
                    }
                    match &tokens[i].0 {
                        Token::OpenBlock { close_idx, .. } => {
                            i = *close_idx - range.start
                        }
                        Token::CloseBlock { .. } => {
                            panic!("Close block should be skipped.");
                        }
                        Token::Operator(_) => {
                            if let Token::OpenBlock { ch, close_idx, .. } =
                                tokens[i + 1].0
                            {
                                if ch == '(' || ch == '{' {
                                    i = close_idx - range.start;
                                } else {
                                    return Err(
                                        CompileError::UnexpectedOperator(
                                            tokens[i].1.clone(),
                                            "$".to_string(),
                                        ),
                                    );
                                }
                            } else {
                                return Err(CompileError::UnexpectedOperator(
                                    tokens[i].1.clone(),
                                    "$".to_string(),
                                ));
                            }
                        }
                        _ => {}
                    }
                    is_binary = true;
                }
            }

            i += 1;
        }

        if let Some((op, i)) = op {
            if op.is_unary() {
                let mut val;
                let mut i = 0;
                while let None = {
                    val = parse_token(
                        &mut i,
                        arc_tokens.clone(),
                        (range.start + 1)..range.end,
                    )?;
                    i += 1;
                    &val
                } {}

                Ok(Expr::FnCall(
                    Box::new(Expr::Var(op.to_fn().to_string())),
                    vec![val.unwrap().await.unwrap()?],
                ))
            } else {
                let arg1 = spawn(parse_math(
                    range.start..(range.start + i),
                    arc_tokens.clone(),
                    _block_level,
                ));
                let arg2 = spawn(parse_math(
                    (range.start + i + 1)..range.end,
                    arc_tokens,
                    _block_level,
                ));
                Ok(Expr::FnCall(
                    Box::new(Expr::Var(op.to_fn().to_string())),
                    vec![arg1.await.unwrap()?, arg2.await.unwrap()?],
                ))
            }
        } else {
            parse_token(&mut 0, arc_tokens, range)?
                .unwrap()
                .await
                .unwrap()
            // (range, arc_tokens, _block_level).await
        }
    }
        .boxed()
}

pub fn parse(
    range: std::ops::Range<usize>,
    arc_tokens: Arc<[(Token, Option<CodePos>)]>,
    _block_level: usize,
) -> BoxFuture<'static, CompileResult<Expr>> {
    async move {
        let tokens = &arc_tokens[range.clone()];
        if let Token::OpenBlock { close_idx, ch, .. } = tokens[0].0 {
            if close_idx + 1 == range.end {
                match ch {
                    '[' => {
                        let mut ret_handlers: Vec<
                            JoinHandle<CompileResult<Expr>>,
                        > = Vec::new();
                        let mut i = 1;
                        while i < tokens.len() - 1 {
                            if let Some(expr_future) = parse_token(
                                &mut i,
                                arc_tokens.clone(),
                                range.clone(),
                            )? {
                                ret_handlers.push(expr_future);
                            }
                            i += 1;
                        }
                        let mut ret: Vec<Expr> =
                            Vec::with_capacity(ret_handlers.len());
                        for handle in ret_handlers {
                            ret.push(handle.await.unwrap()?);
                        }
                        return Ok(Expr::List(ret));
                    }
                    '(' | '{' => {
                        let mut ret_handlers: Vec<
                            Vec<JoinHandle<CompileResult<Expr>>>,
                        > = vec![Vec::new()];
                        let mut i = 1;
                        while i < tokens.len() - 1 {
                            match &tokens[i].0 {
                                Token::Operator(op) if op == ";" => {
                                    ret_handlers.push(Vec::new());
                                }
                                _ => {
                                    if let Some(expr_future) = parse_token(
                                        &mut i,
                                        arc_tokens.clone(),
                                        range.clone(),
                                    )? {
                                        ret_handlers
                                            .last_mut()
                                            .unwrap()
                                            .push(expr_future);
                                    }
                                }
                            }
                            i += 1;
                        }

                        return Ok(if ret_handlers.len() == 0 {
                            if ch == '(' {
                                Expr::FnCall(
                                    Box::new(Expr::Var("@".to_string())),
                                    vec![Expr::Var("none".to_string())],
                                )
                            } else {
                                assert_eq!(ch, '{', "Shouldn't happen!");
                                Expr::Lambda(vec![Expr::FnCall(
                                    Box::new(Expr::Var("@".to_string())),
                                    vec![Expr::Var("none".to_string())],
                                )])
                            }
                        } else {
                            let mut ret =
                                Vec::with_capacity(ret_handlers.len());
                            let ret_handlers_len = ret_handlers.len();
                            for (i, ret_handlers) in
                            ret_handlers.iter_mut().enumerate()
                            {
                                ret_handlers.reverse();
                                let mut none_return = i + 1 == ret_handlers_len;
                                let fn_expr: Box<Expr> =
                                    Box::new(match ret_handlers.pop() {
                                        Some(handle) => {
                                            none_return = false;
                                            handle.await.unwrap()?
                                        }
                                        None => {
                                            if none_return {
                                                if ch == '(' {
                                                    Expr::Var("@".to_string())
                                                } else {
                                                    Expr::Var("ret".to_string())
                                                }
                                            } else {
                                                continue;
                                            }
                                        }
                                    });
                                let mut fn_args: Vec<Expr> =
                                    Vec::with_capacity(ret_handlers.len());
                                if none_return {
                                    // if ch == '{' {
                                    //     // Default return mods
                                    //     fn_args.push(Expr::Var(
                                    //         "none".to_string(),
                                    //     ));
                                    // } else {
                                    //     fn_args.push(Expr::Lambda(vec![
                                    //         Expr::FnCall(
                                    //             Box::new(Expr::Var(
                                    //                 "ret".to_string(),
                                    //             )),
                                    //             vec![Expr::Var(
                                    //                 "none".to_string(),
                                    //             )],
                                    //         ),
                                    //     ]));
                                    // }
                                    fn_args.push(Expr::Var("none".to_string()));
                                } else {
                                    loop {
                                        fn_args.push(
                                            match ret_handlers.pop() {
                                                Some(handle) => {
                                                    handle.await.unwrap()?
                                                }
                                                None => break,
                                            },
                                        );
                                    }
                                }
                                if ret_handlers_len == 1 && ch != '{' {
                                    return Ok(Expr::FnCall(fn_expr, fn_args));
                                }
                                ret.push(Expr::FnCall(fn_expr, fn_args));
                            }
                            match ch {
                                '(' => Expr::Scope(ret),
                                '{' => Expr::Lambda(ret),
                                _ => panic!("Shouldn't happen!"),
                            }
                        });
                    }
                    _ => panic!(
                        "The open block char '{}' isn't an open block char.",
                        ch
                    ),
                }
            }
        }
        if let (
            Token::Operator(op),
            Token::OpenBlock {
                close_idx,
                block_level,
                ch,
            },
        ) = (&tokens[0].0, &tokens[1].0)
        {
            if op == "$" && close_idx + 1 == range.end {
                let res = parse_math(
                    (range.start + 2)..(range.end - 1),
                    arc_tokens.clone(),
                    *block_level,
                )
                    .await?;
                if *ch == '{' {
                    return Ok(Expr::Lambda(vec![res]));
                }
                return Ok(res);
            }
        }
        panic!("Shouldn't happen")
    }
        .boxed()
}
