pub mod lexer;
#[cfg(test)]
mod tests;

use self::lexer::Token;

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}
