use lexer::Token;

// pub const generator: &'static str = "

// ";

#[derive(Debug)]
pub enum ParserErr<'a> {
  UnexpectedIdentifier(i32, &'a str),
  UnexpectedLiteral(i32, &'a str),
  UnexpectedToken(i32, Token),
}