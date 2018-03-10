// use parser::Program;
use parser::Declaration;
use parser::Statement;
use parser::Expression;
use parser::Primary;
use lexer::Literal;
use lexer::Token;

use std::collections::HashMap;

// macro_rules! map(
//   { $($key:expr => $value:expr),+ } => {
//     {
//       let mut m = ::std::collections::HashMap::new();
//         $(
//           m.insert(String::from($key), $value);
//         )+
//       m
//     }
//   };
// );

const NIL: Literal = Literal::Nil;

#[derive(Debug)]
pub enum InterpreterErr {
  TempError,
  ArithmeticErr(String, String, Token, i32),
  IdentifierNotFound(String, i32),
  CastErr(String, String, i32),
}

enum RootType {
  Root(Root),
  RootRef(*mut Root)
}

pub struct Root {
  variables: Vec<Box<Literal>>,
  scopes: Vec<*mut Interpreter>
}

impl Root {
  pub fn new() -> Self {
    Self {
      variables: Vec::new(),
      scopes: Vec::new()
    }
  }

  pub fn gc(&mut self) { // garbage collect

  }
}

pub struct Interpreter {
  variables: HashMap<String, *const Literal>,
  parent: Option<*mut Interpreter>,
  root: RootType
}

impl Interpreter {
  pub fn new() -> Self {
    // let root_obj = if let Some(val) = root {
    //   val
    // } else {
    //   Box::new(Root::new())
    // };

    Self {
      variables: HashMap::new(),
      parent: None,
      root: RootType::Root(Root::new())
    }
  }

  fn new_scoped(parent: *mut Interpreter, root: RootType) -> Self {
    Self {
      variables: HashMap::new(),
      parent: Some(parent),
      root
    }
  }

  pub fn exec(&mut self, program: &Vec<Box<Declaration>>) -> Result<Literal, InterpreterErr> {
    let res = self.exec_program(program)?;
    unsafe {
      Ok((*res).clone())
    }
  }

  fn exec_program(&mut self, program: &Vec<Box<Declaration>>) -> Result<*const Literal, InterpreterErr> {
    let mut last_res: *const Literal = &NIL;

    for i in program {
      last_res = self.exec_decl(i)?;
    }

    Ok(last_res)
  }

  fn exec_decl(&mut self, decl: &Declaration) -> Result<*const Literal, InterpreterErr> {

    match decl {
      &Declaration::Statement(ref stmt, ref _pos) => self.exec_stmt(stmt)
    }
  }

  fn exec_stmt(&mut self, stmt: &Statement) -> Result<*const Literal, InterpreterErr> {
    match stmt {
      &Statement::ExpressionStmt(ref expr, ref is_statement, ref _pos) => {
        let res = self.exec_expr(expr)?;
        if *is_statement {
          Ok(&NIL)
        } else {
          Ok(res)
        }
      }
    }
  }

  fn exec_expr(&mut self, expression: &Expression) -> Result<*const Literal, InterpreterErr> {
    unsafe {
      let res = self.exec_binary(expression)?;
      let res: &Literal = &*res;
      let res = match res {
        &Literal::Variable(ref name) => {
          if let Some(val) = self.get_variable(name as &str) {
            val
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
  }

  fn exec_binary(&mut self, expression: &Expression) -> Result<*const Literal, InterpreterErr> {
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
        self.literal_math(left, right, token, left_pos, right_pos)
      },
      &Expression::Primary(ref literal, _pos) => {
        match literal {
          &Primary::Identifier(ref identifier) => {
            let val_pointer = self.save_value(Literal::Variable(identifier.clone()));
            Ok(val_pointer)
          },
          &Primary::Literal(ref literal) => {
            let val_pointer = self.save_value(literal.clone());
            Ok(val_pointer)
          }
        }
      },
      &Expression::IfExpr(ref expr, ref decls, ref else_decls, ref expr_pos, ref _pos) => {
        let res = self.exec_expr(expr)?;
        let res: bool = self.cast_bool(res, *expr_pos)?;
        let mut scope = self.get_scope();

        let last_item = scope.exec_program(if res {
          decls
        } else {
          else_decls
        })?;

        Ok(last_item)
      },
      &Expression::WhileExpr(ref expr, ref decls, ref expr_pos, ref _pos) => {
        let mut last_item: *const Literal = &NIL;
        let mut scope = self.get_scope();

        loop {
          let res = self.exec_expr(expr)?;
          let res: bool = self.cast_bool(res, *expr_pos)?;

          if !res {
            break;
          }

          last_item = scope.exec_program(decls)?;
        }

        Ok(last_item)
      },
      &Expression::PrintExpr(ref expr, ref _pos) => {
        unsafe {
          let res = &*self.exec_expr(expr)?;
          let to_print = match res {
            &Literal::String(ref s) => String::from(s as &str),
            &Literal::Bool(b) => String::from(if b { "true" } else { "false" }),
            &Literal::Nil => String::from("nil"),
            &Literal::Num(n) => n.to_string(),
            _ => String::from("print not defined for type")
          };

          println!("{}", to_print);
          Ok(res)
        }
      },
      &Expression::FunctionExpr(ref parameters, ref body) => {
        

        Err(InterpreterErr::TempError)
      },
      &Expression::FunctionCallExpr(ref name, ref arguments) => {
        Err(InterpreterErr::TempError)
      }
    }
  }

  fn get_scope(&mut self) -> Interpreter {
    let root_point = self.get_root_point();
    unsafe {
      (*root_point).gc();
    }

    let self_point: *mut Self = self;
    unsafe {
      let scope = Interpreter::new_scoped(self, RootType::RootRef(root_point));
      scope
    }
  }

  fn get_variable(&self, name: &str) -> Option<*const Literal> {
    if let Some(val) = self.variables.get(name) {
      Some(*val)
    } else if let Some(parent) = self.parent {
      unsafe {
        (*parent).get_variable(name)
      }
    } else {
      None
    }
  }

  fn get_root_point(&mut self) -> *mut Root {
    match self.root {
      RootType::Root(ref mut root) => root,
      RootType::RootRef(root) => root
    }
  }

  fn save_value(&mut self, literal: Literal) -> *const Literal {
    unsafe {
      let mut root_point: *mut Root = self.get_root_point();
      (*root_point).variables.push(Box::new(literal));
      &*(*root_point).variables[(*root_point).variables.len() - 1]
    }
  }

  fn find_and_set(&mut self, name: &str, literal: Literal) -> Option<Literal> { // give back literal
    let mut root_point: *mut Root = self.get_root_point();

    unsafe {
      if let Some(parent) = self.parent {
        (*parent).find_and_set(name, literal)
      } else {
        if let Some(_) = self.get_variable(name) {
          (*root_point).variables.push(Box::new(literal));
          let literal_point: *const Literal = &*(*root_point).variables[(*root_point).variables.len() - 1];
          self.variables.insert(name.to_string(), literal_point);

          None
        } else {
          Some(literal)
        }
      }
    }
  }

  fn set_variable(&mut self, name: &str, literal: Literal) {
    unsafe {
      let mut root_point: *mut Root = self.get_root_point();

      if let Some(literal) = self.find_and_set(name, literal) {
        (*root_point).variables.push(Box::new(literal));
        let literal_point: *const Literal = &*(*root_point).variables[(*root_point).variables.len() - 1];
        self.variables.insert(name.to_string(), literal_point);
      }
    }
  }

  fn cast_bool(&self, literal: *const Literal, pos: i32) -> Result<bool, InterpreterErr> {
    unsafe {
      match &*literal {
        &Literal::Bool(b) => Ok(b),
        &Literal::Nil => Ok(false), // temp ?
        &Literal::Num(_) => return Err(InterpreterErr::CastErr("num".to_string(), "bool".to_string(), pos)),
        &Literal::String(_) => return Err(InterpreterErr::CastErr("string".to_string(), "bool".to_string(), pos)),
        &Literal::Function(ref id, ref parameters, _) => return Err(InterpreterErr::CastErr(format!("<function id:{}, ({:?})>", id, parameters), "bool".to_string(), pos)),

        &Literal::Variable(_) => return Err(InterpreterErr::CastErr("variable".to_string(), "bool".to_string(), pos)), // should not happen
      }
    }
  }

  fn literal_math(&mut self, left: *const Literal, right: *const Literal, operation: &(Token, i32), left_pos: i32, right_pos: i32) -> Result<*const Literal, InterpreterErr> {
    let is_assignment = operation.0 == Token::Equals;
    let nleft;
    let nright;

    unsafe {
      nleft = match &*left {
        &Literal::Variable(ref name) => {
          if !is_assignment {
            if let Some(val) = self.get_variable(name as &str) {
              val
            } else {
              &NIL
            }
          } else {
            left.clone()
          }
        },
        _ => left.clone()
      };

      nright = match &*right {
        &Literal::Variable(ref name) => {
          if let Some(val) = self.get_variable(name as &str) {
            val
          } else {
            &NIL
          }
        },
        _ => right.clone()
      };
    

      let literal = match (&*nleft, &*nright, operation.0).clone() {
        // num
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::Plus) => Ok(Literal::Num(i+i2)),
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::Minus) => Ok(Literal::Num(i-i2)),
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::Asterix) => Ok(Literal::Num(i*i2)),
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::Slash) => Ok(Literal::Num(i/i2)),
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::DoubleAsterix) => Ok(Literal::Num(i.powf(*i2))),

        // num compare
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::EqualsEquals) => Ok(Literal::Bool(*i == *i2)),
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::BangEquals) => Ok(Literal::Bool(*i != *i2)),
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::Gt) => Ok(Literal::Bool(*i > *i2)),
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::Lt) => Ok(Literal::Bool(*i < *i2)),
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::GtOrEquals) => Ok(Literal::Bool(*i >= *i2)),
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::LtOrEquals) => Ok(Literal::Bool(*i <= *i2)),

        // string
        (&Literal::String(ref s), &Literal::String(ref s2), Token::Plus) => Ok(Literal::String(format!("{}{}", s, s2))),
        (&Literal::Num(ref i), &Literal::String(ref s), Token::Plus) => Ok(Literal::String(format!{"{}{}", i, s})),
        (&Literal::String(ref s), &Literal::Num(ref i), Token::Plus) => Ok(Literal::String(format!{"{}{}", s, i})),
        (&Literal::String(ref s), &Literal::Num(ref i), Token::Asterix) |
        (&Literal::Num(ref i), &Literal::String(ref s), Token::Asterix) => Ok(Literal::String(format!{"{}", s}.repeat(*i as usize))),

        // string compare
        (&Literal::String(ref s), &Literal::String(ref s2), Token::EqualsEquals) => Ok(Literal::Bool(*s == *s2)),
        (&Literal::String(ref s), &Literal::String(ref s2), Token::BangEquals) => Ok(Literal::Bool(*s != *s2)),

        // boolean
        (&Literal::Num(_), &Literal::Bool(ref b), Token::Bang) => Ok(Literal::Bool(!b)),

        // boolean compare
        (&Literal::Bool(ref b), &Literal::Bool(ref b2), Token::EqualsEquals) => Ok(Literal::Bool(*b == *b2)),
        (&Literal::Bool(ref b), &Literal::Bool(ref b2), Token::BangEquals) => Ok(Literal::Bool(*b != *b2)),

        // assign
        (&Literal::Variable(ref name), &Literal::Num(ref i), Token::Equals) => {
          self.set_variable(name, Literal::Num(*i));
          Ok(Literal::Num(*i))
        },
        (&Literal::Variable(ref name), &Literal::String(ref i), Token::Equals) => {
          self.set_variable(name, Literal::String(i.clone()));
          Ok(Literal::String(i.clone()))
        },
        (&Literal::Variable(ref name), &Literal::Bool(ref i), Token::Equals) => {
          self.set_variable(name, Literal::Bool(*i));
          Ok(Literal::Bool(*i))
        },
        (&Literal::Variable(ref name), &Literal::Nil, Token::Equals) => {
          self.set_variable(name, Literal::Nil);
          Ok(Literal::Nil)
        },

        _ => {
          fn get_type<F>(get_var: &F, literal: *const Literal, pos: i32) -> Result<String, InterpreterErr>
            where F: Fn(&str) -> Option<*const Literal>
          {
            unsafe {
              match &*literal {
                &Literal::Num(_) => Ok("num".to_string()),
                &Literal::String(_) => Ok("string".to_string()),
                &Literal::Nil => Ok("nil".to_string()),
                &Literal::Bool(_) => Ok("boolean".to_string()),
                &Literal::Function(ref id, ref parameters, _) => Ok(format!("<function id:{}, ({:?})>", id, parameters)),
                &Literal::Variable(ref name) => match get_var(name as &str) {
                  Some(val) => get_type(get_var, val, pos),
                  None => {
                    println!("{}", pos);
                    Err(InterpreterErr::IdentifierNotFound(String::from(name as &str), pos))
                    }
                },
              }
            }
          };

          let point: *mut Interpreter = self as *mut Interpreter;
          let get_var = |name: &str| -> Option<*const Literal> {
            unsafe {
              (*point).get_variable(name)
            }
          };

          Err(InterpreterErr::ArithmeticErr(get_type(&get_var, left, left_pos)?, get_type(&get_var, right, right_pos)?, operation.0, operation.1))
        }
      }?;

      Ok(self.save_value(literal))
    }
  }
}