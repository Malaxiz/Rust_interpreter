use lexer::Token;

// pub const generator: &'static str = "

// ";

#[derive(Debug)]
pub enum ParserErr {
  UnexpectedIdentifier(i32),
  UnexpectedLiteral(i32),
  // pos, unexpected_token, expected_tokens, expected_literal, expected_identifier
  UnexpectedToken(i32, Token, Vec<Token>, bool, bool),
  UnexpectedEndOfLine(i32),

  ExpectedSemiColon(i32),
  MismatchedParenthesis(i32),

  GrammarError(i32),
  UnknownErr,
}