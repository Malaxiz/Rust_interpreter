use parser::ParserErr;

use lexer;
use lexer::lex;
use lexer::Lexed;
use lexer::Literal;
use lexer::Token;
use lexer::Token::*;

// not used
// type Program<'a> = Vec<Box<Declaration<'a>>>;

#[derive(Debug)]
pub enum Declaration {
  Statement(Box<Statement>, i32),
  // FunctionDecl(Box<FunctionDecl>, i32),
}

#[derive(Debug)]
pub enum Statement {
  // expression, is statement (semicolon), pos
  ExpressionStmt(Box<Expression>, bool, i32),
  // BlockStmt(Vec<Box<Declaration<'a>>>, i32),
}

#[derive(Debug)]
pub enum Expression {
  Binary(Box<Expression>, (Token, i32), Box<Expression>),
  Primary(Primary, i32),

  // expr, body, else_body, expr_pos, pos
  IfExpr(Box<Expression>, Vec<Box<Declaration>>, Vec<Box<Declaration>>, i32, i32),

  // expr, body, expr_pos, pos
  WhileExpr(Box<Expression>, Vec<Box<Declaration>>, i32, i32),

  // body, pos
  StructExpr(Vec<Box<Declaration>>, i32),

  // struct expression, arguments for __init__, pos
  NewExpr(Box<Expression>, Vec<Box<Expression>>, i32),

  // scope, lookup (identifier), pos
  DotExpr(Box<Expression>, String, i32),

  // parameters, body
  FunctionExpr(Vec<String>, Vec<Box<Declaration>>, i32),

  // function expression, arguments
  FunctionCallExpr(Box<Expression>, Vec<Box<Expression>>, i32)
}

#[derive(Debug, Clone)]
pub enum Primary {
  Literal(lexer::Literal),
  Identifier(String)
}

// pub enum FunctionDecl {
//   Block()
// }

pub struct Grammar {
  lexed: Vec<Lexed>,
  current: usize,
}

impl<'a> Grammar {
  pub fn new(lexed: Vec<Lexed>) -> Self {
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

  pub fn program(&mut self) -> Result<Vec<Box<Declaration>>, ParserErr> {
    let mut declarations: Vec<Box<Declaration>> = vec![];
    while self.current < self.lexed.len() - 1 {
      declarations.push(Box::new(self.declaration()?));
    }

    Ok(
      declarations
    )
  }

  fn declaration(&mut self) -> Result<Declaration, ParserErr> {
    let pos = self.get_pos();
    let stmt = self.statement()?;
    Ok(Declaration::Statement(Box::new(stmt), pos))
  }

  fn statement(&mut self) -> Result<Statement, ParserErr> {
    let pos = self.get_pos();

    // expression statement
    let expr = self.expression()?;
    if let Some((_operator, _pos)) = self.do_match(&[SemiColon]) {
      return Ok(Statement::ExpressionStmt(Box::new(expr), true, pos));
    } else {
      return Ok(Statement::ExpressionStmt(Box::new(expr), false, pos));
      // return Err(ParserErr::ExpectedSemiColon(self.get_pos()));
    }
  }

  pub fn expression(&mut self) -> Result<Expression, ParserErr> {
    let res = self.let_assign()?;
    Ok(res)
  }

  // insert block expr here

  fn let_assign(&mut self) -> Result<Expression, ParserErr> {
    while let Some(_) = self.do_match(&[Let]) {
      let mut expr = self.func_expr()?;
      while let Some((_, pos)) = self.do_match(&[Equals]) {
        let right = self.expression()?;
        expr = Expression::Binary(Box::new(expr), (Let, pos), Box::new(right));
      }
      return Ok(expr);
    }

    Ok(self.assign()?)
  }

  fn assign(&mut self) -> Result<Expression, ParserErr> {
    let mut expr = self.func_expr()?;

    while let Some((operator, pos)) = self.do_match(&[Equals]) {
      let right = self.expression()?;
      expr = Expression::Binary(Box::new(expr), (operator, pos), Box::new(right));
    }

    Ok(expr)
  }
  
  fn func_expr(&mut self) -> Result<Expression, ParserErr> {
    if let Some((_, func_pos)) = self.do_match(&[Func]) {
      let mut parameters: Vec<String> = Vec::new();
      if let Some((_, par_pos)) = self.do_match(&[ParOpen]) {
        loop {
          if let Some(_) = self.do_match(&[Identifier]) {
            if let Lexed::Identifier(ref name, _) = self.lexed[self.current - 1] { // it must be
              parameters.push(name.to_string());
            } else {
              println!("function parsing this should not happen");
            }

            if let None = self.do_match(&[Comma]) {
              break;
            }
          } else {
            break;
            // return Err(ParserErr::ExpectedIdentifier(self.get_pos()));
          }
        }

        if let None = self.do_match(&[ParClose]) {
          println!("here");
          return Err(ParserErr::MismatchedParenthesis(par_pos));
        }
      }

      if let Some(_) = self.do_match(&[BraceOpen]) {
        let mut body: Vec<Box<Declaration>> = Vec::new();
        while let None = self.do_match(&[BraceClose]) {
          body.push(Box::new(self.declaration()?));
        }

        let fexpr = Expression::FunctionExpr(parameters, body, func_pos);
        return Ok(fexpr);
      } else {
        return Err(ParserErr::ExpectedBraceOpen(func_pos));
      }
    }

    Ok(self.while_expr()?)
  }

  fn while_expr(&mut self) -> Result<Expression, ParserErr> {
    let pos = self.get_pos();

    if let Some((_operator, pos)) = self.do_match(&[While]) {
      let expr_pos = self.get_pos();
      let expr = self.expression()?;
      // let expr = Expression::Binary(Box::new(Expression::Primary(Primary::Literal(&Literal::Bool(true)), expr_pos)), (Token::EqualsEquals, expr_pos), Box::new(expr));
      if let None = self.do_match(&[BraceOpen]) {
        return Err(ParserErr::ExpectedBraceOpen(pos));
      }

      let mut decls: Vec<Box<Declaration>> = vec![];

      while let None = self.do_match(&[BraceClose]) {
        decls.push(Box::new(self.declaration()?));
      }

      return Ok(Expression::WhileExpr(Box::new(expr), decls, expr_pos, pos));
    }

    self.if_expr()
  }

  fn if_expr(&mut self) -> Result<Expression, ParserErr> {
    let pos = self.get_pos();

    // if expression
    if let Some((operator, pos)) = self.do_match(&[If]) {
      let expr_pos = self.get_pos();
      let expr = self.expression()?;
      if let None = self.do_match(&[BraceOpen]) {
        return Err(ParserErr::ExpectedBraceOpen(pos));
      }

      let mut decls: Vec<Box<Declaration>> = vec![];
      let mut else_decls: Vec<Box<Declaration>> = vec![];

      while let None = self.do_match(&[BraceClose]) {
        decls.push(Box::new(self.declaration()?));
      }

      if let Some((operator, pos)) = self.do_match(&[Else]) {
        if let None = self.do_match(&[BraceOpen]) {
          return Err(ParserErr::ExpectedBraceOpen(pos));
        }

        while let None = self.do_match(&[BraceClose]) {
          else_decls.push(Box::new(self.declaration()?));
        }
      }

      return Ok(Expression::IfExpr(Box::new(expr), decls, else_decls, expr_pos, pos));
    }

    self.struct_def()
  }

  fn struct_def(&mut self) -> Result<Expression, ParserErr> {
    if let Some((_, pos)) = self.do_match(&[Struct]) {
      if let None = self.do_match(&[BraceOpen]) {
        return Err(ParserErr::ExpectedBraceOpen(pos));
      }

      let lexed = lexer::lex("let __init__ = fn(){};").unwrap();
      let mut this = Self::new(lexed);
      let mut decls: Vec<Box<Declaration>> = this.program()?;

      while let None = self.do_match(&[BraceClose]) {
        decls.push(Box::new(self.declaration()?));
      }

      return Ok(Expression::StructExpr(decls, pos));
    }

    self.equality()
  }

  fn equality(&mut self) -> Result<Expression, ParserErr> {
    let mut expr = self.comparison()?;

    while let Some((operator, pos)) = self.do_match(&[EqualsEquals, BangEquals]) {
      let right = self.comparison()?;
      expr = Expression::Binary(Box::new(expr), (operator, pos), Box::new(right));
    }

    Ok(expr)
  }

  fn comparison(&mut self) -> Result<Expression, ParserErr> {
    let mut expr = self.addition()?;

    while let Some((operator, pos)) = self.do_match(&[Gt, Lt, GtOrEquals, LtOrEquals]) {
      let right = self.addition()?;
      expr = Expression::Binary(Box::new(expr), (operator, pos), Box::new(right));
    }

    Ok(expr)
  }

  fn addition(&mut self) -> Result<Expression, ParserErr> {
    let mut expr = self.multiplication()?;

    while let Some((operator, pos)) = self.do_match(&[Plus, Minus]) {
      let right = self.multiplication()?;
      expr = Expression::Binary(Box::new(expr), (operator, pos), Box::new(right));
    }

    Ok(expr)
  }

  fn multiplication(&mut self) -> Result<Expression, ParserErr> {
    let mut expr = self.raise()?;

    while let Some((operator, pos)) = self.do_match(&[Asterix, Slash]) {
      let right = self.raise()?;
      expr = Expression::Binary(Box::new(expr), (operator, pos), Box::new(right));
    }

    Ok(expr)
  }

  fn raise(&mut self) -> Result<Expression, ParserErr> {
    let mut expr = self.unary()?;

    while let Some((operator, pos)) = self.do_match(&[DoubleAsterix]) {
      let right = self.unary()?;
      expr = Expression::Binary(Box::new(expr), (operator, pos), Box::new(right));
    }

    Ok(expr)
  }

  fn unary(&mut self) -> Result<Expression, ParserErr> {
    if let Some((operator, pos)) = self.do_match(&[Bang, Minus]) {
      let right = self.unary()?;
      return Ok(Expression::Binary(Box::new(Expression::Primary(Primary::Literal(lexer::Literal::Num(0 as f64)), pos)), (operator, pos), Box::new(right)));
    }

    Ok(self.new_expr()?)
  }

  fn new_expr(&mut self) -> Result<Expression, ParserErr> {
    if let Some((_, pos)) = self.do_match(&[New]) {
      let right = self.primary()?;
      let right_pos = self.get_pos();
      if let Some((_, call_pos)) = self.do_match(&[Token::ParOpen]) {
        let mut args: Vec<Box<Expression>> = Vec::new();

        while let None = self.do_match(&[Token::ParClose]) {
          args.push(Box::new(self.expression()?));

          if let None = self.do_match(&[Token::Comma]) {
            if let None = self.do_match(&[Token::ParClose]) {
              return Err(ParserErr::GrammarError(01234));
            } else {
              break;
            }
          }
        }
        return Ok(Expression::NewExpr(Box::new(right), args, pos));
      } else {
        return Err(ParserErr::ExpectedParOpen(right_pos))
      }
    }

    Ok(self.func_call_expr()?)
  }

  fn func_call_expr(&mut self) -> Result<Expression, ParserErr> {
    let mut expr = self.dot_expr()?;

    while let Some((_, call_pos)) = self.do_match(&[Token::ParOpen]) {
      let mut args: Vec<Box<Expression>> = Vec::new();

      while let None = self.do_match(&[Token::ParClose]) {
        args.push(Box::new(self.expression()?));

        if let None = self.do_match(&[Token::Comma]) {
          if let None = self.do_match(&[Token::ParClose]) {
            return Err(ParserErr::GrammarError(01234));
          } else {
            break;
          }
        }
      }
      expr = Expression::FunctionCallExpr(Box::new(expr), args, call_pos);
    }

    Ok(expr)
  }

  fn dot_expr(&mut self) -> Result<Expression, ParserErr> {
    let mut expr = self.primary()?;

    while let Some((_, pos)) = self.do_match(&[Dot]) {
      let right = self.primary()?;
      let right = match right {
        Expression::Primary(ref primary, _) => match primary {
          &Primary::Identifier(ref identifier) => identifier,
          _ => return Err(ParserErr::Temp(69))
        }
        _ => return Err(ParserErr::Temp(68))
      };
      // expr = Expression::Binary(Box::new(expr), op, Box::new(right));
      expr = Expression::DotExpr(Box::new(expr), right.to_string(), pos);
    }

    Ok(expr)
  }

  fn primary(&mut self) -> Result<Expression, ParserErr> {
    let matched = self.do_match(&[Token::Literal, Token::Identifier]);
    if let Some(_) = matched {
      match &self.lexed[self.current - 1] {
        &Lexed::Literal(ref literal, pos) => return Ok(Expression::Primary(Primary::Literal(literal.clone()), pos)),
        &Lexed::Identifier(ref identifier, pos) => return Ok(Expression::Primary(Primary::Identifier(identifier.clone()), pos)),
        _ => return Err(ParserErr::GrammarError(0))
      };
    }

    let matched = self.do_match(&[Token::ParOpen]);
    if let Some((_, pos)) = matched {
      let expr = self.expression();
      if let Some(_) = self.do_match(&[Token::ParClose]) {
        return expr;
      } else {
        println!("{:?}", expr);
        return Err(ParserErr::MismatchedParenthesis(pos));
      }
    }

    if let Some((_, pos)) = self.do_match(&[Token::ParClose]) {
      self.current -= 1;
      return Ok(Expression::Primary(Primary::Literal(Literal::Nil), pos));
    }

    if let Some((_, pos)) = self.do_match(&[Token::EOF]) {
      return Err(ParserErr::UnexpectedToken(pos, Token::EOF, vec![], false, false))
    }

    println!("last effort: {:?}", self.lexed[self.current]);

    // last effort
    let expr = self.expression()?;
    return Ok(expr);

    // println!("{:?}", self.lexed[self.current]);
    // Err(ParserErr::GrammarError(5))
  }
}