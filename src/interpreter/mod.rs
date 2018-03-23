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

enum ExpressionReturnType {
  New(Literal),
  Ref(*const Literal),
  Error(InterpreterErr)
}

use self::ExpressionReturnType::*;

enum RootType {
  Root(Root),
  RootRef(*mut Root)
}

enum GcSize {
  Easy,
  Hard,
}

pub struct Root {
  variables: Vec<Box<Literal>>,
  scopes: Vec<*mut Interpreter>,

  variable_stack: Vec<*const Literal>
}

impl Root {
  pub fn new() -> Self {
    Self {
      variables: Vec::new(),
      scopes: Vec::new(),
      variable_stack: Vec::new()
    }
  }

  fn gc(&mut self, size: GcSize) { // garbage collect
    unsafe {
      let self_point: *mut Self = self;
      self.variables.retain(|ref x| match ***x {
        Literal::Variable(_) => {
          let a = (*self_point).variable_stack.iter().any(|ref y| **y == &(***x) as *const Literal);
          // println!("{}", a);
          a
        },
        _ => true
      });
    }

    // if let GcSize::Hard = size {
    //   self.variables.retain(|ref x| match ***x {

    //   })
    // }
  }
}

pub struct Interpreter {
  variables: HashMap<String, *const Literal>,
  parent: Option<*mut Interpreter>,
  root: RootType
}

impl Drop for Interpreter {
  fn drop(&mut self) {
    println!("dropping: {:?}", self as *mut Interpreter);
  }
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
    println!("{:?}", *program);
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
        unsafe {
          (*self.get_root_point()).variable_stack.push(left);
        }

        let right = self.exec_binary(&*right)?;
        let result = self.literal_math(left, right, token, left_pos, right_pos);

        unsafe {
          let root_point = self.get_root_point();
          (*root_point).gc(GcSize::Easy);
          (*root_point).variable_stack.retain(|ref x| **x == left);
        }

        result
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
        let mut decls: Vec<*const Declaration> = Vec::new();
        for ref i in body {
          decls.push(&***i as *const Declaration);
        }
        let func = Literal::Function((*parameters).clone(), decls);
        
        Ok(self.save_value(func))
        // Err(InterpreterErr::TempError)
      },
      &Expression::FunctionCallExpr(ref name, ref arguments) => {
        Err(InterpreterErr::TempError)
      }
    }
  }

  fn get_scope(&mut self) -> Box<Interpreter> {
    let root_point = self.get_root_point();
    unsafe {
      // (*root_point).gc(GcSize::Hard);
    }

    let mut scope = Box::new(Interpreter::new_scoped(self, RootType::RootRef(root_point)));
    let scope_point: *mut Interpreter = &mut *scope;
    unsafe {
      (*root_point).scopes.push(scope_point);
    }
    scope
  }

  fn get_variable(&mut self, name: &str) -> Option<*const Literal> {
    if name == "vars" { // temp?
      unsafe {
        let self_point: *mut Self = self;
        return Some(self.save_value(Literal::String(format!("{:?}", (*(*self_point).get_root_point()).variables))));
      }
    }

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
      let root_point: *mut Root = self.get_root_point();
      let b = Box::new(literal);
      let point: *const Literal = &*b;
      (*root_point).variables.push(b);
      point
    }
  }

  fn find_and_set(&mut self, name: &str, literal: *const Literal) -> Option<()> { // give back literal
    unsafe {
      if let Some(parent) = self.parent {
        (*parent).find_and_set(name, literal)
      } else {
        if let Some(_) = self.get_variable(name) {
          self.variables.insert(name.to_string(), literal);
          Some(())
        } else {
          None
        }
      }
    }
  }

  fn set_variable(&mut self, name: &str, literal: *const Literal) {
    if let None = self.find_and_set(name, literal) {
      self.variables.insert(name.to_string(), literal);
    }
  }

  fn cast_bool(&self, literal: *const Literal, pos: i32) -> Result<bool, InterpreterErr> {
    unsafe {
      match &*literal {
        &Literal::Bool(b) => Ok(b),
        &Literal::Nil => Ok(false), // temp ?
        &Literal::Num(_) => return Err(InterpreterErr::CastErr("num".to_string(), "bool".to_string(), pos)),
        &Literal::String(_) => return Err(InterpreterErr::CastErr("string".to_string(), "bool".to_string(), pos)),
        &Literal::Function(ref parameters, _) => return Err(InterpreterErr::CastErr(format!("<function ({:?})>", parameters), "bool".to_string(), pos)),

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
            left
          }
        },
        _ => left
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
    
      

      let literal_return: ExpressionReturnType = match (&*nleft, &*nright, operation.0).clone() {
        // num
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::Plus) => New(Literal::Num(i+i2)),
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::Minus) => New(Literal::Num(i-i2)),
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::Asterix) => New(Literal::Num(i*i2)),
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::Slash) => New(Literal::Num(i/i2)),
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::DoubleAsterix) => New(Literal::Num(i.powf(*i2))),

        // num compare
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::EqualsEquals) => New(Literal::Bool(*i == *i2)),
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::BangEquals) => New(Literal::Bool(*i != *i2)),
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::Gt) => New(Literal::Bool(*i > *i2)),
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::Lt) => New(Literal::Bool(*i < *i2)),
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::GtOrEquals) => New(Literal::Bool(*i >= *i2)),
        (&Literal::Num(ref i), &Literal::Num(ref i2), Token::LtOrEquals) => New(Literal::Bool(*i <= *i2)),

        // string
        (&Literal::String(ref s), &Literal::String(ref s2), Token::Plus) => New(Literal::String(format!("{}{}", s, s2))),
        (&Literal::Num(ref i), &Literal::String(ref s), Token::Plus) => New(Literal::String(format!{"{}{}", i, s})),
        (&Literal::String(ref s), &Literal::Num(ref i), Token::Plus) => New(Literal::String(format!{"{}{}", s, i})),
        (&Literal::String(ref s), &Literal::Num(ref i), Token::Asterix) |
        (&Literal::Num(ref i), &Literal::String(ref s), Token::Asterix) => New(Literal::String(format!{"{}", s}.repeat(*i as usize))),

        // string compare
        (&Literal::String(ref s), &Literal::String(ref s2), Token::EqualsEquals) => New(Literal::Bool(*s == *s2)),
        (&Literal::String(ref s), &Literal::String(ref s2), Token::BangEquals) => New(Literal::Bool(*s != *s2)),

        // boolean
        (&Literal::Num(_), &Literal::Bool(ref b), Token::Bang) => New(Literal::Bool(!b)),

        // boolean compare
        (&Literal::Bool(ref b), &Literal::Bool(ref b2), Token::EqualsEquals) => New(Literal::Bool(*b == *b2)),
        (&Literal::Bool(ref b), &Literal::Bool(ref b2), Token::BangEquals) => New(Literal::Bool(*b != *b2)),

        (&Literal::Variable(ref name), _, Token::Equals) => {
          self.set_variable(name, nright);
          // println!("vars: {:?}", (*self.get_root_point()).variables);
          Ref(nright)
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
                &Literal::Function(ref parameters, _) => Ok(format!("<function ({:?})>", parameters)),
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

          println!("err: {:?}, {:?}", *left, *right);

          let point: *mut Interpreter = self as *mut Interpreter;
          let get_var = |name: &str| -> Option<*const Literal> {
            (*point).get_variable(name)
          };

          Error(InterpreterErr::ArithmeticErr(get_type(&get_var, left, left_pos)?, get_type(&get_var, right, right_pos)?, operation.0, operation.1))
        }
      };

      match literal_return {
        ExpressionReturnType::Error(err) => Err(err),
        ExpressionReturnType::New(literal) => Ok(self.save_value(literal)),
        ExpressionReturnType::Ref(reference) => Ok(reference)
      }
    }
  }
}