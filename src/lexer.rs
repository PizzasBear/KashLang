use std::str::Chars;

use crate::{CompileError, CompileResult, error::CodePos};
use crate::lexer::Token::CloseBlock;
use crate::parse::expr::Literal;

#[derive(Debug)]
pub enum Token {
    Id(String),
    Operator(String),
    Literal(Literal),
    OpenBlock {
        block_level: usize,
        ch: char,
        close_idx: usize,
    },
    CloseBlock {
        block_level: usize,
        ch: char,
        open_idx: usize,
    },
    NewLine,
}

#[derive(Eq, PartialEq)]
enum LiteralType {
    Id,
    Operator,
    Str(CodePos),
    Int,
    UInt,
    Float,
    Comment,
}

fn where_contains<'a, T, U>(a: U, cmp_el: &T) -> Option<usize>
where
    T: PartialEq + 'static,
    U: IntoIterator<Item=&'a T>,
{
    for (i, el) in a.into_iter().enumerate() {
        if *el == *cmp_el {
            return Some(i);
        }
    }
    None
}

const OPEN_BLOCK: [char; 3] = ['(', '{', '['];
const CLOSE_BLOCK: [char; 3] = [')', '}', ']'];
const IGNORABLE: [char; 4] = [' ', '\n', '\r', '\t'];

fn push_token(
    ch: char,
    literal_type: &mut Option<LiteralType>,
    tokens: &mut Vec<(Token, CodePos)>,
    literal: &mut String,
    blocks: &mut Vec<(usize, usize)>,
    code_pos: &CodePos,
) -> CompileResult<()> {
    if let Some(literal_type) = literal_type {
        match literal_type {
            LiteralType::Float => match literal.parse() {
                Ok(f) => tokens.push((
                    Token::Literal(Literal::Float(f)),
                    CodePos {
                        column: code_pos.column - literal.len(),
                        line: code_pos.line,
                    },
                )),
                Err(_) => return Err(CompileError::ParseFloat(*code_pos)),
            },
            LiteralType::Int => match literal.parse() {
                Ok(i) => tokens.push((
                    Token::Literal(Literal::Int(i)),
                    CodePos {
                        column: code_pos.column - literal.len(),
                        line: code_pos.line,
                    },
                )),
                Err(_) => return Err(CompileError::ParseInt(*code_pos)),
            },
            LiteralType::UInt => match literal.parse() {
                Ok(u) => tokens.push((
                    Token::Literal(Literal::UInt(u)),
                    CodePos {
                        column: code_pos.column - literal.len() - 1,
                        line: code_pos.line,
                    },
                )),
                Err(_) => return Err(CompileError::ParseInt(*code_pos)),
            },
            LiteralType::Id => {
                tokens.push((
                    Token::Id((*literal).clone()),
                    CodePos {
                        column: code_pos.column - literal.len(),
                        line: code_pos.line,
                    },
                ));
            }
            LiteralType::Operator => tokens.push((
                Token::Operator((*literal).clone()),
                CodePos {
                    column: code_pos.column - literal.len(),
                    line: code_pos.line,
                },
            )),
            LiteralType::Str(pos) => tokens.push((
                Token::Literal(Literal::Str((*literal).clone())),
                *pos,
            )),
            LiteralType::Comment => {
                panic!("Comment token should never be pushed.")
            }
        }
    } else if let Some(open_idx) = where_contains(&OPEN_BLOCK, &ch) {
        blocks.push((tokens.len(), open_idx));
        tokens.push((
            Token::OpenBlock {
                block_level: blocks.len(),
                ch,
                close_idx: 0,
            },
            *code_pos,
        ));
    } else if let Some(close_idx) = where_contains(&CLOSE_BLOCK, &ch) {
        if let Some((open_token_idx, open_idx)) = blocks.pop() {
            if open_idx != close_idx {
                return Err(CompileError::MismatchedClosingDelimiter(
                    *code_pos,
                    OPEN_BLOCK[open_idx],
                    ch,
                ));
            }
            let tokens_len = tokens.len();
            if let Token::OpenBlock { close_idx, .. } =
            &mut tokens[open_token_idx].0
            {
                *close_idx = tokens_len;
            } else {
                panic!("The token that was referenced in `blocks` wasn't `Token::OpenBlock`.");
            }
            tokens.push((
                Token::CloseBlock {
                    block_level: blocks.len() + 1,
                    ch,
                    open_idx: open_token_idx,
                },
                *code_pos,
            ));
        } else {
            return Err(CompileError::UnexpectedClosingDelimiter(
                *code_pos, ch,
            ));
        }
    }

    *literal_type = None;
    literal.clear();

    Ok(())
}

pub async fn lex(
    mut code: Chars<'_>,
) -> CompileResult<Vec<(Token, CodePos)>> {
    let mut tokens = vec![(
        Token::OpenBlock {
            block_level: 0,
            ch: '(',
            close_idx: 0,
        },
        CodePos {
            line: 0,
            column: 0,
        },
    )];
    let mut literal = String::new();
    let mut literal_type = None::<LiteralType>;
    let mut blocks = Vec::new();
    let mut code_pos = CodePos { line: 1, column: 1 };

    let mut prev_ch = ['\x00', '\x00'];
    let mut option_ch = code.next();
    while let Some(ch) = option_ch {
        // ignore '\r'
        if ch == '\r' {
            option_ch = code.next()
        }

        // if `literal_type` was determined then match the `LiteralType`
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
                            &mut blocks,
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
                            &mut blocks,
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
                            &mut blocks,
                            &code_pos,
                        )?;
                        continue;
                    }
                },
                LiteralType::Int => match ch {
                    '0'..='9' => literal.push(ch),
                    _ => {
                        if literal == "-" {
                            tokens.push((
                                Token::Operator("-".to_string()),
                                code_pos,
                            ));
                            literal.clear();
                            literal_type = None;
                            continue;
                        }
                        match ch {
                            '.' => {
                                literal.push(ch);
                                literal_type = Some(LiteralType::Float);
                            }
                            'u' => literal_type = Some(LiteralType::UInt),
                            'f' => literal_type = Some(LiteralType::Float),
                            _ => {
                                if ('a'..='z').contains(&ch)
                                    | ('A'..='Z').contains(&ch)
                                {
                                    return Err(CompileError::ParseInt(
                                        code_pos,
                                    ));
                                }
                                push_token(
                                    ch,
                                    &mut literal_type,
                                    &mut tokens,
                                    &mut literal,
                                    &mut blocks,
                                    &code_pos,
                                )?;
                                continue;
                            }
                        }
                    }
                },
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
                        &mut blocks,
                        &code_pos,
                    )?;
                    continue;
                }
                LiteralType::Str(_) => {
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
                            &mut blocks,
                            &code_pos,
                        )?;
                    } else if ch != '\\' {
                        literal.push(ch)
                    }
                }
                LiteralType::Comment => {
                    if ch == '\n' {
                        prev_ch = ['\x00', '\x00'];
                        literal.clear();
                        literal_type = None;
                    }
                }
            }
        } else {
            match ch {
                'a'..='z' | 'A'..='Z' | '_' => {
                    literal_type = Some(LiteralType::Id);
                    literal.push(ch);
                }
                '0'..='9' | '-' => {
                    literal_type = Some(LiteralType::Int);
                    literal.push(ch);
                }
                '"' => literal_type = Some(LiteralType::Str(code_pos)),
                '@' => {
                    literal.clear();
                    tokens.push((Token::Id("@".to_string()), code_pos));
                }
                '#' => literal_type = Some(LiteralType::Comment),
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
                            &mut blocks,
                            &code_pos,
                        )?;
                    }
                }
            }
        }

        // `code.next` + increment code_pos
        code_pos.column += 1;
        if ch == '\n' {
            tokens.push((
                Token::NewLine,
                CodePos {
                    column: code_pos.column - 1,
                    line: code_pos.line,
                },
            ));
            code_pos.line += 1;
            code_pos.column = 1;
        }

        prev_ch[0] = prev_ch[1];
        prev_ch[1] = ch;

        option_ch = code.next();
    }

    if let Some(some_literal_type) = &literal_type {
        if let LiteralType::Str(_) = some_literal_type {
            return Err(CompileError::UnclosedStringLiteral(code_pos));
        }
        push_token(
            '\n',
            &mut literal_type,
            &mut tokens,
            &mut literal,
            &mut blocks,
            &code_pos,
        )?;
    }

    let tokens_len = tokens.len();
    if let Token::OpenBlock { close_idx, .. } = &mut tokens[0].0 {
        *close_idx = tokens_len;
    } else {
        panic!("Shouldn't happen!")
    }
    tokens.push((
        CloseBlock {
            block_level: 0,
            ch: ')',
            open_idx: 0,
        },
        CodePos {
            line: 0,
            column: 0,
        },
    ));

    Ok(tokens)
}
