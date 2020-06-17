use crate::parse::expr::Literal;
use crate::{error::CodePos, CompileError, CompileResult};
use std::str::Chars;

#[derive(Debug)]
pub enum Token {
    Id(String),
    Operator(String),
    Literal(Literal),
    OpenBlock(usize, char),
    CloseBlock(usize, char),
}

#[derive(Eq, PartialEq)]
enum LiteralType {
    Id,
    Operator,
    Str,
    Int,
    UInt,
    Float,
}

fn where_contains<'a, T, U>(a: U, cmp_el: &T) -> Option<usize>
where
    T: PartialEq + 'static,
    U: IntoIterator<Item = &'a T>,
{
    for (i, el) in a.into_iter().enumerate() {
        if *el == *cmp_el {
            return Some(i);
        }
    }
    None
}

pub async fn lex(mut code: Chars<'_>) -> CompileResult<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut literal = String::new();
    let mut literal_type = None::<LiteralType>;
    let mut blocks = Vec::new();
    let mut code_pos = CodePos { line: 1, column: 1 };

    const OPEN_BLOCK: [char; 3] = ['(', '{', '['];
    const CLOSE_BLOCK: [char; 3] = [')', '}', ']'];
    const IGNORABLE: [char; 4] = [' ', '\n', '\r', '\t'];

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
                LiteralType::Id => tokens.push(Token::Id((*literal).clone())),
                LiteralType::Operator => {
                    tokens.push(Token::Operator((*literal).clone()))
                }
                LiteralType::Str => tokens
                    .push(Token::Literal(Literal::Str((*literal).clone()))),
            }
        } else if let Some(open_idx) = where_contains(&OPEN_BLOCK, &ch) {
            blocks.push(open_idx);
            tokens.push(Token::OpenBlock(blocks.len(), ch));
        } else if let Some(close_idx) = where_contains(&CLOSE_BLOCK, &ch) {
            if let Some(open_idx) = blocks.pop() {
                if open_idx != close_idx {
                    return Err(CompileError::MismatchedClosingDelimiter(
                        *code_pos,
                        OPEN_BLOCK[open_idx],
                        ch,
                    ));
                }
            } else {
                return Err(CompileError::UnexpectedClosingDelimiter(
                    *code_pos, ch,
                ));
            }
            tokens.push(Token::CloseBlock(blocks.len(), ch));
        }

        *literal_type = None;
        literal.clear();

        Ok(())
    };

    let mut prev_ch = ['\x00', '\x00'];
    let mut option_ch = code.next();
    while let Some(ch) = option_ch {
        // ignore '\r'
        if ch == '\r' {
            option_ch = code.next()
        }

        // if `literal_type` was determened then match the `LiteralType`
        if let Some(some_literal_type) = &literal_type {
            match some_literal_type {
                LiteralType::Id => match ch {
                    // If `ch` can be in the id
                    'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                        literal.push(ch);
                    }
                    // else push the token and reinterpret the char
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
                // very similar to Id
                LiteralType::Operator => {
                    if ch == '"'
                        || ch == '_'
                        || ('0'..='9').contains(&ch)
                        || ('A'..='Z').contains(&ch)
                        || ('a'..='z').contains(&ch)
                        || OPEN_BLOCK.contains(&ch)
                        || CLOSE_BLOCK.contains(&ch)
                        || IGNORABLE.contains(&ch)
                    {
                        push_token(
                            ch,
                            &mut literal_type,
                            &mut tokens,
                            &mut literal,
                            &code_pos,
                        )?;
                        continue;
                    } else {
                        literal.push(ch);
                    }
                }
                LiteralType::Float => match ch {
                    '0'..='9' => literal.push(ch),
                    _ => {
                        if ('a'..='z').contains(&ch) | ('A'..='Z').contains(&ch)
                        {
                            return Err(CompileError::ParseInt(code_pos));
                        }
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
                    'u' => literal_type = Some(LiteralType::UInt),
                    'f' => literal_type = Some(LiteralType::Float),
                    _ => {
                        if ('a'..='z').contains(&ch) | ('A'..='Z').contains(&ch)
                        {
                            return Err(CompileError::ParseInt(code_pos));
                        }
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
                    if ('a'..='z').contains(&ch)
                        | ('A'..='Z').contains(&ch)
                        | ('0'..='9').contains(&ch)
                    {
                        return Err(CompileError::ParseInt(code_pos));
                    }
                    push_token(
                        ch,
                        &mut literal_type,
                        &mut tokens,
                        &mut literal,
                        &code_pos,
                    )?;
                    continue;
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
                _ => {
                    if !OPEN_BLOCK.contains(&ch)
                        && !CLOSE_BLOCK.contains(&ch)
                        && !IGNORABLE.contains(&ch)
                    {
                        literal_type = Some(LiteralType::Operator);
                        literal.push(ch);
                    } else {
                        push_token(
                            ch,
                            &mut literal_type,
                            &mut tokens,
                            &mut literal,
                            &code_pos,
                        )?;
                    }
                }
            }
        }

        // `code.next` + increment code_pos
        code_pos.column += 1;
        if ch == '\n' {
            code_pos.line += 1;
            code_pos.column = 1;
        }

        prev_ch[0] = prev_ch[1];
        prev_ch[1] = ch;

        option_ch = code.next();
    }

    if let Some(some_literal_type) = &literal_type {
        if LiteralType::Str == *some_literal_type {
            return Err(CompileError::UnclosedStringLiteral(code_pos))
        }
        push_token(
            '\n',
            &mut literal_type,
            &mut tokens,
            &mut literal,
            &code_pos,
        )?;
    }

    Ok(tokens)
}
