#![allow(dead_code)]

pub use error::CompileError;

pub mod error;
pub mod lexer;
pub mod parse;

pub type CompileResult<T> = Result<T, CompileError>;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let tokens =
        lexer::lex(String::from_utf8(std::fs::read("file.fmt")?)?.chars())
            .await?;
    println!("[");
    for token in tokens.iter() {
        println!("\t{:?},", token);
    }
    println!("]");
    Ok(())
}
