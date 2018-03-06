// use parser::Program;
use parser::Declaration;
use parser::Statement;
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
  CastErr(&'static str, &'static str, i32),
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

  pub fn exec_program(&mut self, program: &Vec<Box<Declaration<'a>>>) -> Result<Literal, InterpreterErr> {
    let mut last_res: Literal = Literal::Nil;

    for i in program {
      last_res = self.exec_decl(i)?;
    }

    Ok(last_res)
  }

  fn exec_decl(&mut self, decl: &Declaration) -> Result<Literal, InterpreterErr> {

    match decl {
      &Declaration::Statement(ref stmt, ref _pos) => self.exec_stmt(stmt)
    }
  }

  fn exec_stmt(&mut self, stmt: &Statement) -> Result<Literal, InterpreterErr> {

    match stmt {
      &Statement::ExpressionStmt(ref expr, ref is_statement, ref _pos) => {
        let res = self.exec_expr(expr)?;
        if *is_statement {
          Ok(Literal::Nil)
        } else {
          Ok(res)
        }
      }
    }
  }

  fn exec_expr(&mut self, expression: &Expression<'a>) -> Result<Literal, InterpreterErr> {
    let res = self.exec_binary(expression)?;
    let res = match res {
      Literal::Variable(ref name) => {
        if let Some(val) = self.variables.get(name as &str) {
          val.clone()
        } else {
          let pos = match expression {
            &Expression::Primary(_, ref pos) => *pos,
            _ => 0
          };
          return Err(InterpreterErr::IdentifierNotFound(String::from(name as &str), pos)) // temp design choice ?
          // Literal::Nil
        }
      },
      _ => res
    };
    
    Ok(res)
  }

  fn exec_binary(&mut self, expression: &Expression<'a>) -> Result<Literal, InterpreterErr> {
    match expression {
      &Expression::Binary(ref left, ref token, ref right) => {
        let left_pos = match **left {
          Expression::Primary(_, pos) => pos,
          _ => 0
        };
        let right_pos = match **right {
          Expression::Primary(_, pos) => pos,
          _ => 0
        };

        let left = self.exec_binary(&*left)?;
        let right = self.exec_binary(&*right)?;
        self.literal_math(&left, &right, token, left_pos, right_pos)
      },
      &Expression::Primary(ref literal, _pos) => {
        match literal {
          &Primary::Identifier(identifier) => Ok(Literal::Variable(String::from(identifier))),
          &Primary::Literal(literal) => Ok(literal.clone())
        }
      },
      &Expression::IfExpr(ref expr, ref decls, ref else_decls, ref expr_pos, ref _pos) => {
        let res = self.exec_expr(expr)?;
        let res: bool = self.cast_bool(&res, *expr_pos)?;

        let last_item = self.exec_program(if res {
          decls
        } else {
          else_decls
        })?;

        Ok(last_item)
      },
      &Expression::WhileExpr(ref expr, ref decls, ref expr_pos, ref _pos) => {
        let mut last_item = Literal::Nil;

        loop {
          let res = self.exec_expr(expr)?;
          let res: bool = self.cast_bool(&res, *expr_pos)?;

          if !res {
            break;
          }

          last_item = self.exec_program(decls)?;
        }

        Ok(last_item)
      },
      &Expression::PrintExpr(ref expr, ref _pos) => {
        let res = self.exec_expr(expr)?;
        let to_print = match res {
          Literal::String(ref s) => String::from(s as &str),
          Literal::Bool(b) => String::from(if b { "true" } else { "false" }),
          Literal::Nil => String::from("nil"),
          Literal::Num(n) => n.to_string(),
          _ => String::from("should not happen")
        };

        println!("{}", to_print);
        Ok(res)
      }
    }
  }

  fn cast_bool(&self, literal: &Literal, pos: i32) -> Result<bool, InterpreterErr> {
    match literal {
      &Literal::Bool(b) => Ok(b),
      &Literal::Nil => Ok(false), // temp ?
      &Literal::Num(_) => return Err(InterpreterErr::CastErr("num", "bool", pos)),
      &Literal::String(_) => return Err(InterpreterErr::CastErr("string", "bool", pos)),
      &Literal::Variable(_) => return Err(InterpreterErr::CastErr("variable", "bool", pos)), // should not happen
    }
  }

  fn literal_math(&mut self, left: &Literal, right: &Literal, operation: &(Token, i32), left_pos: i32, right_pos: i32) -> Result<Literal, InterpreterErr> {
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
      // num
      (Literal::Num(ref i), Literal::Num(ref i2), Token::Plus) => Ok(Literal::Num(i+i2)),
      (Literal::Num(ref i), Literal::Num(ref i2), Token::Minus) => Ok(Literal::Num(i-i2)),
      (Literal::Num(ref i), Literal::Num(ref i2), Token::Asterix) => Ok(Literal::Num(i*i2)),
      (Literal::Num(ref i), Literal::Num(ref i2), Token::Slash) => Ok(Literal::Num(i/i2)),
      (Literal::Num(ref i), Literal::Num(ref i2), Token::DoubleAsterix) => Ok(Literal::Num(i.powf(*i2))),

      // num compare
      (Literal::Num(ref i), Literal::Num(ref i2), Token::EqualsEquals) => Ok(Literal::Bool(*i == *i2)),
      (Literal::Num(ref i), Literal::Num(ref i2), Token::BangEquals) => Ok(Literal::Bool(*i != *i2)),
      (Literal::Num(ref i), Literal::Num(ref i2), Token::Gt) => Ok(Literal::Bool(*i > *i2)),
      (Literal::Num(ref i), Literal::Num(ref i2), Token::Lt) => Ok(Literal::Bool(*i < *i2)),
      (Literal::Num(ref i), Literal::Num(ref i2), Token::GtOrEquals) => Ok(Literal::Bool(*i >= *i2)),
      (Literal::Num(ref i), Literal::Num(ref i2), Token::LtOrEquals) => Ok(Literal::Bool(*i <= *i2)),

      // string
      (Literal::String(ref s), Literal::String(ref s2), Token::Plus) => Ok(Literal::String(format!("{}{}", s, s2))),
      (Literal::String(ref s), Literal::Num(ref i), Token::Asterix) |
      (Literal::Num(ref i), Literal::String(ref s), Token::Asterix) => Ok(Literal::String(format!{"{}", s}.repeat(*i as usize))),

      // string compare
      (Literal::String(ref s), Literal::String(ref s2), Token::EqualsEquals) => Ok(Literal::Bool(*s == *s2)),
      (Literal::String(ref s), Literal::String(ref s2), Token::BangEquals) => Ok(Literal::Bool(*s != *s2)),

      // boolean
      (Literal::Num(_), Literal::Bool(ref b), Token::Bang) => Ok(Literal::Bool(!b)),

      // boolean compare
      (Literal::Bool(ref b), Literal::Bool(ref b2), Token::EqualsEquals) => Ok(Literal::Bool(*b == *b2)),
      (Literal::Bool(ref b), Literal::Bool(ref b2), Token::BangEquals) => Ok(Literal::Bool(*b != *b2)),

      // assign
      (Literal::Variable(ref name), Literal::Num(ref i), Token::Equals) => {
        self.variables.insert(String::from(name as &str), Literal::Num(*i));
        Ok(Literal::Num(*i))
      },
      (Literal::Variable(ref name), Literal::String(ref i), Token::Equals) => {
        self.variables.insert(String::from(name as &str), Literal::String(i.clone()));
        Ok(Literal::String(i.clone()))
      },
      (Literal::Variable(ref name), Literal::Bool(ref i), Token::Equals) => {
        self.variables.insert(String::from(name as &str), Literal::Bool(*i));
        Ok(Literal::Bool(*i))
      },
      (Literal::Variable(ref name), Literal::Nil, Token::Equals) => {
        self.variables.insert(String::from(name as &str), Literal::Nil);
        Ok(Literal::Nil)
      },

      _ => {
        fn get_type(variables: &HashMap<String, Literal>, literal: &Literal, pos: i32) -> Result<&'static str, InterpreterErr> {
          match literal {
            &Literal::Num(_) => Ok("num"),
            &Literal::String(_) => Ok("string"),
            &Literal::Nil => Ok("nil"),
            &Literal::Bool(_) => Ok("boolean"),
            &Literal::Variable(ref name) => match variables.get(name as &str) {
              Some(val) => match val {
                &Literal::Num(_) => Ok("num"),
                &Literal::String(_) => Ok("string"),
                &Literal::Nil => Ok("nil"),
                &Literal::Bool(_) => Ok("boolean"),
                _ => get_type(variables, val, pos)
              },
              None => Err(InterpreterErr::IdentifierNotFound(String::from(name as &str), pos))
            },
          }
        };

        Err(InterpreterErr::ArithmeticErr(get_type(&self.variables, left, left_pos)?, get_type(&self.variables, right, right_pos)?, operation.0, operation.1))
      }
    }
  }
}