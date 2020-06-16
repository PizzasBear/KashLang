use crate::parse::expr::Literal;
use crate::{error::CodePos, CompileError, CompileResult};
use std::str::Chars;

#[derive(Debug)]
pub enum Token {
    Id(String),
    Operator(char),
    Literal(Literal),
    BlockStart(usize, char),
    BlockEnd(usize, char),
}

#[derive(Eq, PartialEq)]
enum LiteralType {
    Id,
    Str,
    Int,
    UInt,
    Float,
}

pub fn lex(mut code: Chars) -> CompileResult<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut literal = String::new();
    let mut literal_type = None::<LiteralType>;
    let mut block_lvl = 0;
    let mut code_pos = CodePos { line: 0, column: 0 };

    let mut push_token = |ch: char,
                          literal_type: &mut Option<LiteralType>,
                          tokens: &mut Vec<Token>,
                          literal: &mut String,
                          code_pos: &CodePos|
     -> CompileResult<()> {
        if let Some(literal_type) = literal_type {
            match literal_type {
                LiteralType::Float => match literal.parse() {
                    Ok(f) => tokens.push(Token::Literal(Literal::Float(f))),
                    Err(_) => return Err(CompileError::ParseFloat(*code_pos)),
                },
                LiteralType::Int => match literal.parse() {
                    Ok(i) => tokens.push(Token::Literal(Literal::Int(i))),
                    Err(_) => return Err(CompileError::ParseInt(*code_pos)),
                },
                LiteralType::UInt => match literal.parse() {
                    Ok(u) => tokens.push(Token::Literal(Literal::UInt(u))),
                    Err(_) => return Err(CompileError::ParseInt(*code_pos)),
                },
                LiteralType::Id => tokens.push(Token::Id(literal.clone())),

                LiteralType::Str => {
                    tokens.push(Token::Literal(Literal::Str(literal.clone())))
                }
            }
        } else {
            match ch {
                '(' | '[' | '{' => {
                    tokens.push(Token::BlockStart(block_lvl, ch));
                    block_lvl += 1;
                }
                ')' | ']' | '}' => {
                    block_lvl -= 1;
                    tokens.push(Token::BlockEnd(block_lvl, ch));
                }
                ' ' => {} // ignorable
                _ => tokens.push(Token::Operator(ch)),
            }
        }

        *literal_type = None;
        literal.clear();

        Ok(())
    };

    let mut prev_ch = ['\x00', '\x00'];
    let mut option_ch = code.next();
    while let Some(ch) = option_ch {
        if let Some(some_literal_type) = &literal_type {
            match some_literal_type {
                LiteralType::Id => match ch {
                    'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                        literal.push(ch);
                    }
                    _ => {
                        push_token(
                            ch,
                            &mut literal_type,
                            &mut tokens,
                            &mut literal,
                            &code_pos,
                        )?;
                        continue;
                    }
                },

                LiteralType::Float => match ch {
                    '0'..='9' => literal.push(ch),
                    _ => {
                        push_token(
                            ch,
                            &mut literal_type,
                            &mut tokens,
                            &mut literal,
                            &code_pos,
                        )?;
                        continue;
                    }
                },
                LiteralType::Int => match ch {
                    '0'..='9' => literal.push(ch),
                    '.' => {
                        literal.push(ch);
                        literal_type = Some(LiteralType::Float);
                    }
                    'u' => {
                        literal_type = Some(LiteralType::UInt);
                        push_token(
                            ch,
                            &mut literal_type,
                            &mut tokens,
                            &mut literal,
                            &code_pos,
                        )?;
                    }
                    'f' => {
                        literal_type = Some(LiteralType::Float);
                        push_token(
                            ch,
                            &mut literal_type,
                            &mut tokens,
                            &mut literal,
                            &code_pos,
                        )?;
                    }
                    'i' => push_token(
                        ch,
                        &mut literal_type,
                        &mut tokens,
                        &mut literal,
                        &code_pos,
                    )?,
                    _ => {
                        push_token(
                            ch,
                            &mut literal_type,
                            &mut tokens,
                            &mut literal,
                            &code_pos,
                        )?;
                        continue;
                    }
                },
                // This shouldn't happen because UInts are Ints until the 'u' suffix
                LiteralType::UInt => {
                    panic!("Somehow there was a UInt at the literal building")
                }
                LiteralType::Str => {
                    if prev_ch[1] == '\\' && prev_ch[0] != '\\' {
                        match ch {
                            '\\' => literal.push('\\'),
                            'n' => literal.push('\n'),
                            '"' => literal.push('\"'),
                            't' => literal.push('\t'),
                            'r' => literal.push('\r'),
                            '0' => literal.push('\0'),
                            _ => {}
                        }
                    } else if ch == '\"' {
                        push_token(
                            ch,
                            &mut literal_type,
                            &mut tokens,
                            &mut literal,
                            &code_pos,
                        )?;
                    } else if ch != '\\' {
                        literal.push(ch)
                    }
                }
            }
        } else {
            match ch {
                'a'..='z' | 'A'..='Z' | '_' => {
                    literal_type = Some(LiteralType::Id);
                    literal.push(ch);
                }
                '0'..='9' => {
                    literal_type = Some(LiteralType::Int);
                    literal.push(ch);
                }
                '"' => literal_type = Some(LiteralType::Str),
                _ => push_token(
                    ch,
                    &mut literal_type,
                    &mut tokens,
                    &mut literal,
                    &code_pos,
                )?,
            }
        }

        // iter next + increment code_pos
        code_pos.column += 1;
        if ch == '\n' {
            code_pos.line += 1;
            code_pos.column = 0;
        }

        prev_ch[0] = prev_ch[1];
        prev_ch[1] = ch;

        option_ch = code.next();
    }

    Ok(tokens)
}
