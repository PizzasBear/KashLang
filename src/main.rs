#![allow(dead_code)]

pub mod error;
pub mod lexer;
pub mod parse;

pub use error::CompileError;

pub type CompileResult<T> = Result<T, CompileError>;

#[tokio::main]
async fn main() {
    println!("Hello, world!");
}
