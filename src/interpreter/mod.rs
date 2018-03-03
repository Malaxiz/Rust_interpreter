use parser::Expression;
use parser::Primary;
use lexer::Literal;
use lexer::Token;

#[derive(Debug)]
pub enum InterpreterErr {
  InterpreterError,
}

pub struct Interpreter {

}

impl<'a> Interpreter {
  pub fn new() -> Self {
    Interpreter { }
  }

  pub fn exec_expr(&self, expression: &Expression<'a>) -> Result<String, InterpreterErr> {
    Ok(format!("{:?}", self.exec_binary(expression)?))
  }

  fn exec_binary(&'a self, expression: &Expression<'a>) -> Result<Literal, InterpreterErr> {
    match expression {
      &Expression::Binary(ref left, ref token, ref right) => {
        let left = self.exec_binary(&*left)?;
        let right = self.exec_binary(&*right)?;
        self.do_math(&left, &right, &token.0)
      },
      &Expression::Primary(ref literal, _pos) => {
        match literal {
          &Primary::Identifier(_) => Err(InterpreterErr::InterpreterError),
          &Primary::Literal(literal) => Ok(literal.clone())
        }
      }
    }
  }

  fn do_math(&self, left: &Literal, right: &Literal, operation: &Token) -> Result<Literal, InterpreterErr> {
    match (left, right, operation) {
      (&Literal::Num(ref i), &Literal::Num(ref i2), &Token::Plus) => Ok(Literal::Num(i+i2)),
      (&Literal::Num(ref i), &Literal::Num(ref i2), &Token::Minus) => Ok(Literal::Num(i-i2)),
      (&Literal::Num(ref i), &Literal::Num(ref i2), &Token::Asterix) => Ok(Literal::Num(i*i2)),
      (&Literal::Num(ref i), &Literal::Num(ref i2), &Token::Slash) => Ok(Literal::Num(i/i2)),

      (&Literal::String(ref s), &Literal::String(ref s2), &Token::Plus) => Ok(Literal::String(format!("{}{}", s, s2))),
      _ => Err(InterpreterErr::InterpreterError)
    }
  }
}