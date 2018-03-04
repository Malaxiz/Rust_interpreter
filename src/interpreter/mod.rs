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
          m.insert(String::from($key), $value);
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

pub struct Interpreter {
  variables: HashMap<String, Literal>
}

impl<'a> Interpreter {
  pub fn new() -> Self {
    Interpreter {
      variables: map![
        "dab" => Literal::String(String::from("yayayaya"))
      ]
    }
  }

  pub fn exec_expr(&mut self, expression: &Expression<'a>) -> Result<String, InterpreterErr> {
    let res = self.exec_binary(expression)?;
    let res = match res {
      Literal::Variable(ref name) => {
        if let Some(val) = self.variables.get(name as &str) {
          val.clone()
        } else {
          Literal::Nil
        }
      },
      _ => res
    };
    Ok(format!("{:?}", res))
  }

  fn exec_binary(&mut self, expression: &Expression<'a>) -> Result<Literal, InterpreterErr> {
    match expression {
      &Expression::Binary(ref left, ref token, ref right) => {
        let left = self.exec_binary(&*left)?;
        let right = self.exec_binary(&*right)?;
        self.literal_math(&left, &right, token)
      },
      &Expression::Primary(ref literal, _pos) => {
        match literal {
          &Primary::Identifier(identifier) => {
            // if let Some(val) = self.variables.get(identifier) {
              // Ok(val.clone())
            // } else {
              // Err(InterpreterErr::IdentifierNotFound(String::from(identifier), pos))
              Ok(Literal::Variable(String::from(identifier)))
            // }
          },
          &Primary::Literal(literal) => Ok(literal.clone())
        }
      }
    }
  }

  fn literal_math(&mut self, left: &Literal, right: &Literal, operation: &(Token, i32)) -> Result<Literal, InterpreterErr> {
    let is_assignment = operation.0 == Token::Equals;

    let nleft;
    let nright;

    {
      nleft = match left {
        &Literal::Variable(ref name) => {
          if !is_assignment {
            if let Some(val) = self.variables.get(name as &str) {
              val
            } else {
              &Literal::Nil
            }
          } else {
            left
          }
        },
        _ => left
      }.clone();

      nright = match right {
        &Literal::Variable(ref name) => {
          if let Some(val) = self.variables.get(name as &str) {
            val
          } else {
            &Literal::Nil
          }
        },
        _ => right
      }.clone();
    }

    match (nleft, nright, operation.0).clone() {
      (Literal::Num(ref i), Literal::Num(ref i2), Token::Plus) => Ok(Literal::Num(i+i2)),
      (Literal::Num(ref i), Literal::Num(ref i2), Token::Minus) => Ok(Literal::Num(i-i2)),
      (Literal::Num(ref i), Literal::Num(ref i2), Token::Asterix) => Ok(Literal::Num(i*i2)),
      (Literal::Num(ref i), Literal::Num(ref i2), Token::Slash) => Ok(Literal::Num(i/i2)),

      (Literal::String(ref s), Literal::String(ref s2), Token::Plus) => Ok(Literal::String(format!("{}{}", s, s2))),
      (Literal::String(ref s), Literal::Num(ref i), Token::Asterix) => Ok(Literal::String(format!{"{}", s}.repeat(*i as usize))),

      (Literal::Variable(ref name), Literal::Num(ref i), Token::Equals) => {
        self.variables.insert(String::from(name as &str), Literal::Num(*i));
        println!("{:?}", self.variables);
        Ok(Literal::Num(*i))
      },

      _ => {
        fn get_type(variables: &HashMap<String, Literal>, literal: &Literal) -> &'static str {
          match literal {
            &Literal::Num(_) => "num",
            &Literal::String(_) => "string",
            &Literal::Nil => "nil",
            &Literal::Bool(_) => "boolean",
            &Literal::Variable(ref name) => match variables.get(name as &str) {
              Some(val) => match val {
                &Literal::Num(_) => "num",
                &Literal::String(_) => "string",
                &Literal::Nil => "nil",
                &Literal::Bool(_) => "boolean",
                _ => get_type(variables, val)
              },
              None => "nil"
            },
          }
        };

        Err(InterpreterErr::ArithmeticErr(get_type(&self.variables, left), get_type(&self.variables, right), operation.0, operation.1))
      }
    }
  }
}