use parser::ParserErr;

use lexer;
use lexer::Lexed;
use lexer::Token;
use lexer::Token::*;

#[derive(Debug)]
pub enum Expression<'a> {
  Binary(Box<Expression<'a>>, (Token, i32), Box<Expression<'a>>),
  Primary(Primary<'a>, i32)
}

#[derive(Debug)]
pub enum Primary<'a> {
  Literal(&'a lexer::Literal),
  Identifier(&'a str)
}

pub struct Grammar<'a> {
  lexed: &'a Vec<Lexed>,
  current: usize,
}

impl<'a> Grammar<'a> {
  pub fn new(lexed: &'a Vec<Lexed>) -> Self {
    Grammar {
      lexed,
      current: 0,
    }
  }

  fn do_match(&mut self, tokens: &[Token]) -> Option<(Token, i32)> {
    if self.lexed.len() <= 0 { // ?
      return None;
    }

    let tokens = tokens.into_iter();

    let (token, pos) = match &self.lexed[self.current] {
      &Lexed::Literal(_, pos) => (Token::Literal, pos),
      &Lexed::Identifier(_, pos) => (Token::Identifier, pos),
      &Lexed::Operator(token, pos) => (token, pos)
    };

    for i in tokens {
      if *i == token {
        self.advance();
        return Some((token.clone(), pos));
      }
    }

    None
  }

  fn advance(&mut self) {
    self.current += 1;
  }

  pub fn expression(&mut self) -> Result<Expression<'a>, ParserErr> {
    self.addition()
  }

  fn addition(&mut self) -> Result<Expression<'a>, ParserErr> {
    let mut expr = self.multiplication()?;

    while let Some((operator, pos)) = self.do_match(&[Plus, Minus]) {
      let right = self.multiplication()?;
      expr = Expression::Binary(Box::new(expr), (operator, pos), Box::new(right));
    }

    Ok(expr)
  }

  fn multiplication(&mut self) -> Result<Expression<'a>, ParserErr> {
    let mut expr = self.primary()?;

    while let Some((operator, pos)) = self.do_match(&[Asterix, Slash]) {
      let right = self.primary()?;
      expr = Expression::Binary(Box::new(expr), (operator, pos), Box::new(right));
    }

    Ok(expr)
  }

  fn primary(&mut self) -> Result<Expression<'a>, ParserErr> {
    let matched = self.do_match(&[Token::Literal, Token::Identifier]);
    if let Some(_) = matched {
      match &self.lexed[self.current - 1] {
        &Lexed::Literal(ref literal, pos) => return Ok(Expression::Primary(Primary::Literal(literal), pos)),
        &Lexed::Identifier(ref identifier, pos) => return Ok(Expression::Primary(Primary::Identifier(identifier), pos)),
        _ => return Err(ParserErr::GrammarError(0))
      };
    }

    Err(ParserErr::GrammarError(0))
  }

}