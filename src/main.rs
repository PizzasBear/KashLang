#![allow(dead_code)]

pub use error::CompileError;

pub mod error;
pub mod lexer;
pub mod parse;

pub type CompileResult<T> = Result<T, CompileError>;

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

    println!("{:?}", parse::parse(0..tokens.len(), tokens, 0).await?);

    Ok(())
}
