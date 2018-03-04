use parser::ParserErr;

use lexer;
use lexer::Lexed;
use lexer::Token;
use lexer::Token::*;

// not used
pub struct Program<'a>(Vec<Box<Declaration<'a>>>);

#[derive(Debug)]
pub enum Declaration<'a> {
  Statement(Box<Statement<'a>>, i32),
  // FunctionDecl(Box<FunctionDecl>, i32),
}

#[derive(Debug)]
pub enum Statement<'a> {
  ExpressionStmt(Box<Expression<'a>>, i32),
  PrintStmt(Box<Expression<'a>>, i32),
  // BlockStmt(Vec<Box<Declaration<'a>>>, i32),
}

#[derive(Debug)]
pub enum Expression<'a> {
  Binary(Box<Expression<'a>>, (Token, i32), Box<Expression<'a>>),
  Primary(Primary<'a>, i32)
}

#[derive(Debug, Clone)]
pub enum Primary<'a> {
  Literal(&'a lexer::Literal),
  Identifier(&'a str)
}

// pub enum FunctionDecl {
//   Block()
// }

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

  fn get_pos(&self) -> i32 {
    match self.lexed[self.current] {
      Lexed::Identifier(_, pos) => pos,
      Lexed::Literal(_, pos) => pos,
      Lexed::Operator(_, pos) => pos
    }
  }

  pub fn program(&mut self) -> Result<Vec<Box<Declaration<'a>>>, ParserErr> {
    let mut declarations: Vec<Box<Declaration>> = vec![];
    while self.current < self.lexed.len() - 1 {
      declarations.push(Box::new(self.declaration()?));
    }

    Ok(
      declarations
    )
  }

  fn declaration(&mut self) -> Result<Declaration<'a>, ParserErr> {
    let pos = self.get_pos();
    let stmt = self.statement()?;
    Ok(Declaration::Statement(Box::new(stmt), pos))
  }

  fn statement(&mut self) -> Result<Statement<'a>, ParserErr> {
    let pos = self.get_pos();

    if let Some((operator, pos)) = self.do_match(&[Print]) {
      let expr = self.expression()?;
      if let None = self.do_match(&[SemiColon]) {
        return Err(ParserErr::ExpectedSemiColon(self.get_pos()))
      } else {
        return Ok(Statement::PrintStmt(Box::new(expr), pos))
      }
    }

    let expr = self.expression()?;
    if let Some((_operator, pos)) = self.do_match(&[SemiColon]) {
      return Ok(Statement::ExpressionStmt(Box::new(expr), pos))
    } else {
      return Err(ParserErr::ExpectedSemiColon(self.get_pos()))
    }
  }

  pub fn expression(&mut self) -> Result<Expression<'a>, ParserErr> {
    self.assign()
  }

  fn assign(&mut self) -> Result<Expression<'a>, ParserErr> {
    let mut expr = self.equality()?;

    while let Some((operator, pos)) = self.do_match(&[Equals]) {
      let right = self.assign()?;
      expr = Expression::Binary(Box::new(expr), (operator, pos), Box::new(right));
    }

    Ok(expr)
  }

  fn equality(&mut self) -> Result<Expression<'a>, ParserErr> {
    let mut expr = self.comparison()?;

    while let Some((operator, pos)) = self.do_match(&[EqualsEquals, BangEquals]) {
      let right = self.comparison()?;
      expr = Expression::Binary(Box::new(expr), (operator, pos), Box::new(right));
    }

    Ok(expr)
  }

  fn comparison(&mut self) -> Result<Expression<'a>, ParserErr> {
    let mut expr = self.addition()?;

    while let Some((operator, pos)) = self.do_match(&[Gt, Lt, GtOrEquals, LtOrEquals]) {
      let right = self.addition()?;
      expr = Expression::Binary(Box::new(expr), (operator, pos), Box::new(right));
    }

    Ok(expr)
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
    let mut expr = self.raise()?;

    while let Some((operator, pos)) = self.do_match(&[Asterix, Slash]) {
      let right = self.raise()?;
      expr = Expression::Binary(Box::new(expr), (operator, pos), Box::new(right));
    }

    Ok(expr)
  }

  fn raise(&mut self) -> Result<Expression<'a>, ParserErr> {
    let mut expr = self.unary()?;

    while let Some((operator, pos)) = self.do_match(&[DoubleAsterix]) {
      let right = self.unary()?;
      expr = Expression::Binary(Box::new(expr), (operator, pos), Box::new(right));
    }

    Ok(expr)
  }

  fn unary(&mut self) -> Result<Expression<'a>, ParserErr> {
    if let Some((operator, pos)) = self.do_match(&[Bang, Minus]) {
      let right = self.unary()?;
      return Ok(Expression::Binary(Box::new(Expression::Primary(Primary::Literal(&lexer::Literal::Num(0 as f64)), pos)), (operator, pos), Box::new(right)));
    }

    Ok(self.primary()?)
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

    let matched = self.do_match(&[Token::ParOpen]);
    if let Some(_) = matched {
      let expr = self.expression();
      if let None = self.do_match(&[Token::ParClose]) {
        let pos = match self.lexed[self.current - 1] {
          Lexed::Literal(_, pos) => pos,
          Lexed::Identifier(_, pos) => pos,
          Lexed::Operator(_, pos) => pos
        };
        return Err(ParserErr::MismatchedParenthesis(pos));
      }
      return expr;
    }

    Err(ParserErr::GrammarError(5))
  }

}