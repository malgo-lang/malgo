#![feature(
    assert_matches,
    exclusive_range_pattern,
    io_read_to_string,
    io_error_other
)]

mod eval;
mod parser;
#[cfg(test)]
mod tests;

use std::io;

use clap::Parser as ClapParser;
use parser::{language::language, Parser, PrintSyntaxNode};

#[derive(ClapParser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {}

fn main() -> io::Result<()> {
    let _args = Args::parse();
    let input = io::read_to_string(io::stdin())?;

    let _ = tracing_subscriber::fmt::try_init();

    let parser = Parser::new(&input, language());
    let root = match parser.parse(Parser::expr) {
        Ok(it) => it,
        Err(err) => return Err(io::Error::other(err.to_string())),
    };

    println!("{}", PrintSyntaxNode(&root));

    Ok(())
}
