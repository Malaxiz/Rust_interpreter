use parser::Expression;
use parser::Primary;
use lexer::Literal;
use lexer::Token;

use std::collections::HashMap;

macro_rules! map(
  { $($key:expr => $value:expr),+ } => {
    {
      let mut m = ::std::collections::HashMap::new();
        $(
          m.insert($key, $value);
        )+
      m
    }
  };
);

#[derive(Debug)]
pub enum InterpreterErr {
  TempError,
  ArithmeticErr(&'static str, &'static str, Token, i32),
  IdentifierNotFound(String, i32),
}

pub struct Interpreter<'a> {
  variables: HashMap<&'a str, Literal>
}

impl<'a> Interpreter<'a> {
  pub fn new() -> Self {
    Interpreter {
      variables: map![
        "dab" => Literal::String(String::from("yayayaya"))
      ]
    }
  }

  pub fn exec_expr(&self, expression: &Expression<'a>) -> Result<String, InterpreterErr> {
    Ok(format!("{:?}", self.exec_binary(expression)?))
  }

  fn exec_binary(&'a self, expression: &Expression<'a>) -> Result<Literal, InterpreterErr> {
    match expression {
      &Expression::Binary(ref left, ref token, ref right) => {
        let left = self.exec_binary(&*left)?;
        let right = self.exec_binary(&*right)?;
        self.literal_math(&left, &right, token)
      },
      &Expression::Primary(ref literal, pos) => {
        match literal {
          &Primary::Identifier(identifier) => {
            if let Some(val) = self.variables.get(identifier) {
              Ok(val.clone())
            } else {
              Err(InterpreterErr::IdentifierNotFound(String::from(identifier), pos))
            }
          },
          &Primary::Literal(literal) => Ok(literal.clone())
        }
      }
    }
  }

  fn literal_math(&self, left: &Literal, right: &Literal, operation: &(Token, i32)) -> Result<Literal, InterpreterErr> {
    let get_type = |literal: &Literal| -> &'static str {
      match literal {
        &Literal::Num(_) => "num",
        &Literal::String(_) => "string",
        &Literal::Nil => "nil",
        &Literal::Bool(_) => "boolean"
      }
    };

    match (left, right, operation.0) {
      (&Literal::Num(ref i), &Literal::Num(ref i2), Token::Plus) => Ok(Literal::Num(i+i2)),
      (&Literal::Num(ref i), &Literal::Num(ref i2), Token::Minus) => Ok(Literal::Num(i-i2)),
      (&Literal::Num(ref i), &Literal::Num(ref i2), Token::Asterix) => Ok(Literal::Num(i*i2)),
      (&Literal::Num(ref i), &Literal::Num(ref i2), Token::Slash) => Ok(Literal::Num(i/i2)),

      (&Literal::String(ref s), &Literal::String(ref s2), Token::Plus) => Ok(Literal::String(format!("{}{}", s, s2))),
      (&Literal::String(ref s), &Literal::Num(ref i), Token::Asterix) => Ok(Literal::String(format!{"{}", s}.repeat(*i as usize))),

      _ => Err(InterpreterErr::ArithmeticErr(get_type(left), get_type(right), operation.0, operation.1))
    }
  }
}