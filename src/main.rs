#![allow(dead_code)]

use std::collections::HashMap;

pub use error::{CompileError, RuntimeError};

use crate::interpreter::interpret;

pub mod error;
pub mod interpreter;
pub mod lexer;
pub mod parse;

pub type CompileResult<T> = Result<T, CompileError>;
pub type RuntimeResult<T> = Result<T, RuntimeError>;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let tokens: std::sync::Arc<[_]> =
        lexer::lex(std::fs::read_to_string("file.fmt")?.chars())
            .await?
            .into();
    println!("tokens: [");
    for token in tokens.iter() {
        println!("\t{:?},", token);
    }
    println!("]");
    println!();

    let ast = parse::parse(0..tokens.len(), tokens, 0).await?;
    println!("{:?}", ast);
    println!();
    println!("Program Output:");
    interpret(&ast, &mut HashMap::new(), &mut Vec::new())?;

    Ok(())
}
