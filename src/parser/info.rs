use lexer::Token;

// pub const generator: &'static str = "

// ";

#[derive(Debug)]
pub enum ParserErr<'a> {
  UnexpectedIdentifier(i32, &'a str),
  UnexpectedLiteral(i32, &'a str),
  // pos, unexpected_token, expected_tokens, expected_literal, expected_identifier
  UnexpectedToken(i32, Token, Vec<Token>, bool, bool),
  UnexpectedEndOfLine(i32),
}