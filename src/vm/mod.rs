extern crate num;
use self::num::FromPrimitive;

pub use super::parser;
pub use parser::Declaration;
pub use parser::Statement;
pub use parser::Expression;
pub use parser::Primary;

pub use super::lexer;
pub use lexer::Token;
// pub use lexer::Literal;

use self::OPCode::*;
use self::VMBuildError::*;
use self::VMExecError::*;

use std::str;
use std::slice;
use std::mem;
use std::fs::File;
use std::io::prelude::*;

pub type Program = Vec<Operation>;
pub type Decls = Vec<Box<Declaration>>;

const NIL: *const Value = &Value::NIL;

fn u(op_code: OPCode) -> u8 {
  op_code as u8
}

#[derive(Debug)]
pub enum VMBuildError {
  // error, pos
  InvalidExpression(String, i32),
  UnsupportedOperator(Token, i32),
  Temp
}

#[derive(Debug)]
pub enum VMExecError {
  // error, pos
  UnsupportedOperation(Literal, Literal, OPCode, i32),
  InvalidOPCode(String),
  UnsupportedOPCode(String),
  Temp
}

// description of opcode
// structure in release mode
// structure in debug mode
enum_from_primitive! {
  #[derive(Debug, Clone, Copy, PartialEq)]
  pub enum OPCode {
    NULL = 0x00,
    END = 0x01,

    VERSION,
    DEBUG,

    META_END,

    DEBUG_CODE,
    DEBUG_CODE_END,

    // pushes an int.
    // ( int )
    // 
    PUSH_NUM,

    POP,

    PEEK,

    // operation on top two stack values.
    // ( )
    // ( pos )
    ASSIGN,
    ADD,
    SUB,
    MULTIPLY,
  }
}

bitflags! {
  pub struct BuildOptions: u32 {
    const DEBUG = 0x01;
    const PROGRESS = 0x02;
  }
}

#[derive(Debug)]
pub struct Operation {
  code: Option<OPCode>,
  val: u8,
}

#[derive(Copy, Clone, Debug)]
pub enum Literal {
  Num(f64),

  // Pos of string
  // String(i32)
}

#[derive(Copy, Clone, Debug)]
enum Value {
  // Variable(),
  Literal(Literal),
  NIL,

  None,
}

struct Root {
  pool: Vec<Box<Value>>
}

impl Root {
  pub fn new() -> Self {
    Self {
      pool: Vec::new()
    }
  }

  pub fn gc() {
    // todo
  }
}

struct VMExec {
  op_i: usize,
  program: Program,

  root: Root,
  stack: [*const Value; 512],
  stacki: usize,

  query: String
}

impl VMExec {
  pub fn new() -> Self {
    Self {
      op_i: 0,
      program: Vec::new(),

      root: Root::new(),
      stack: [&Value::None; 512],
      stacki: 0,

      query: String::from("")
    }
  }

  fn reset(&mut self) {
    self.program = Vec::new();
    self.op_i = 0;
    self.stack = [&Value::None; 512];
    self.stacki = 0;
    self.query = String::from("");
  }

  fn consume(&mut self) -> u8 {
    self.op_i += 1;
    self.program[self.op_i].val
  }

  fn stack_push(&mut self, val: *const Value) {
    self.stack[self.stacki] = val;
    self.stacki += 1;
  }

  fn stack_pop(&mut self) -> *const Value {
    if self.stacki <= 0 {
      return NIL;
    }
    self.stacki -= 1;
    self.stack[self.stacki]
  }

  fn literal_operation(&mut self, val1: *const Value, val2: *const Value, operation: &OPCode, pos: Option<i32>) -> Result<*const Value, VMExecError> {
    let get_pos = || {
      match pos {
        Some(pos) => pos,
        _ => -1
      }
    };

    unsafe {
      match (*val1, *val2) {
        (Value::Literal(val1), Value::Literal(val2)) => {
          let res: Value = match (val1, val2, operation) {
            (Literal::Num(first), Literal::Num(second), &ADD) => {
              Value::Literal(Literal::Num(first + second))
            },
            (Literal::Num(first), Literal::Num(second), &SUB) => {
              Value::Literal(Literal::Num(first - second))
            },
            (Literal::Num(first), Literal::Num(second), &MULTIPLY) => {
              Value::Literal(Literal::Num(first * second))
            },
            _ => return Err(VMExecError::UnsupportedOperation(val1, val2, operation.clone(), get_pos()))
          };

          let res = Box::new(res);
          let res_point: *const Value = &*res;
          self.root.pool.push(res);
          Ok(res_point)
        },
        _ => return Err(VMExecError::Temp)
      }
    }
  }

  fn print_stack(self) {
    print!("stack: {}\n---------\n", self.stacki);
    for (i, v) in self.stack.iter().enumerate() {
      unsafe {
        if *v != &Value::None {
          print!("    {}: {:?}\n", i, **v);
        }
      }
    }
    print!("---------\n");
  }

  fn exec(&mut self, program: Program) -> Result<String, VMExecError> {
    self.reset();
    // self.program = program;

    let mut program = vec![];
    let mut f = File::open("foo.ops").unwrap();
    for byte in f.bytes() {
      program.push(byte.unwrap());
    }

    let program: Program = program.iter().map(|c| Operation {
      code: OPCode::from_i32(*c as i32),
      val: *c
    }).collect();

    println!("{:#?}", program);

    self.program = program; // temp

    let mut meta_end = false;
    let mut is_debug = false;

    let mut self_point: *mut Self = self;

    loop {
      let op: &Operation = unsafe {
        &((*self_point).program[self.op_i])
      };
      let code: &Option<OPCode> = &op.code;

      if let &Some(ref code) = code {
        if !meta_end {
          match *code {
            META_END => meta_end = true,
            DEBUG => is_debug = true,
            DEBUG_CODE => {
              let mut query: Vec<u8> = Vec::new();
              loop {
                self.op_i += 1;
                let op: &Operation = unsafe {
                  &((*self_point).program[self.op_i])
                };
                let code: &Option<OPCode> = &op.code;
                if let &Some(ref code) = code {
                  if code == &DEBUG_CODE_END {
                    break;
                  }
                }
                
                query.push(op.val);
              }

              let query = match str::from_utf8(&query) {
                Ok(v) => v,
                Err(e) => panic!("Invalid UTF-8 sequence: {}", e),
              };

              self.query = query.to_string();
            },
            _ => {}
          }
          
          self.op_i += 1;
          continue;
        }
        
        match *code {
          END => {
            unsafe {
              return Ok(format!("{:?}", *self.stack_pop()));
            }
          },
          PUSH_NUM => {
            let mut num: [u8; 8] = [0x00; 8];
            for k in 0..8 {
              num[k] = self.consume();
            }

            let num = unsafe {
              mem::transmute::<[u8; 8], f64>(num)
            };

            let val = Box::new(Value::Literal(Literal::Num(num)));
            let val_point = &*val as *const Value;
            self.root.pool.push(val);
            self.stack_push(val_point);
          },
          ADD | SUB | MULTIPLY => {
            let mut pos = None;
            if is_debug {
              pos = Some(self.consume() as i32);
            }

            // println!("pos: {}", pos);

            let second = self.stack_pop();
            let first = self.stack_pop();
            let res = self.literal_operation(first, second, code, pos)?;
            self.stack_push(res);
          },
          POP => {
            self.stack_pop();
          }
          _ => return Err(VMExecError::UnsupportedOPCode(format!("{:?}", op.val)))
        };

        self.op_i += 1;
      } else {
        return Err(VMExecError::InvalidOPCode(format!("{:?}", op.val)))
      }
    }
    

  }
}

struct VMBuild {
  program: Vec<u8>,
  is_debug: bool
}

impl VMBuild {
  pub fn new() -> Self {
    Self {
      program: Vec::new(),
      is_debug: true
    }
  }

  fn build_binary(&mut self, expr: &Expression, pos: i32) -> Result<Vec<u8>, VMBuildError> {
    match expr {
      &Expression::Binary(ref left, ref token, ref right) => {
        let left_pos = match **left {
          Expression::Primary(_, pos) => pos,
          _ => 0
        };
        let right_pos = match **right {
          Expression::Primary(_, pos) => pos,
          _ => 0
        };

        let mut left = self.build_binary(&*left, left_pos)?;
        let mut right = self.build_binary(&*right, right_pos)?;

        left.append(&mut right);

        left.push(u(match token {
          &(Token::Plus, pos) => ADD,
          &(Token::Minus, pos) => SUB,
          &(Token::Asterix, pos) => MULTIPLY,
          &(_, pos) => return Err(VMBuildError::UnsupportedOperator(token.0, pos))
        }));

        let pos = match token {
          &(_, pos) => pos
        };

        println!("pos: {:?}", pos);

        left.push(pos as u8);
        Ok(left)
      },
      &Expression::Primary(ref literal, _pos) => {
        match literal {
          &Primary::Identifier(ref identifier) => {
            // println!("identifier: {}", identifier);
            // let val_pointer = self.save_value(Literal::Variable(identifier.clone()));
            // Ok(val_pointer)
            Ok(vec![u(PUSH_NUM), 0x00])
          },
          &Primary::Literal(ref literal) => {
            // let val_pointer = self.save_value(literal.clone());
            // Ok(val_pointer)
            match literal {
              &lexer::Literal::Num(num) => {
                let bv: [u8; 8] = unsafe {
                  mem::transmute(num)
                };
                let mut bv: Vec<u8> = bv.to_vec();

                let mut v = vec![u(PUSH_NUM)];
                v.append(&mut bv);
                Ok(v)
              },
              _ => Err(VMBuildError::Temp)
            }
          }
        }
      },
      _ => Err(VMBuildError::InvalidExpression(format!("{:?}", expr), pos))
    }
  }

  fn build_expr(&mut self, expr: &Expression, pos: i32) -> Result<Vec<u8>, VMBuildError> {
    let binary = self.build_binary(expr, pos);
    binary
  }

  fn build_stmt(&mut self, stmt: &Statement) -> Result<Vec<u8>, VMBuildError> {
    match stmt {
      &Statement::ExpressionStmt(ref expr, ref is_statement, pos) => self.build_expr(expr, pos)
    }
  }

  fn build_decl(&mut self, decl: &Declaration) -> Result<Vec<u8>, VMBuildError> {
    match decl {
      &Declaration::Statement(ref stmt, pos) => self.build_stmt(stmt)
    }
  }

  pub fn build(&mut self, decls: Decls, query: String, options: BuildOptions) -> Result<Program, VMBuildError> {
    // let mut program: Vec<u8> = vec![VERSION as u8, 0x01, DEBUG as u8, META_END as u8,
    //                                 PUSH_INT as u8, 0x01, PUSH_INT as u8, 0x01, SUB as u8, 0x02,
    //                                 END as u8,
    //                                 DEBUG_CODE as u8,
    //                                 '1' as u8, '+' as u8, '1' as u8, ';' as u8,
    //                                 DEBUG_CODE_END as u8];

    // let mut program: Vec<u8> = vec![u(VERSION), 0x01, u(DEBUG), u(META_END)];

    let mut program: Vec<u8> = vec![u(VERSION), 0x01, u(DEBUG), u(DEBUG_CODE)];
    let mut code = query.as_bytes().to_vec();
    program.append(&mut code);
    program.push(u(DEBUG_CODE_END));
    program.push(u(META_END));


    for i in decls {
      println!("{:?}", i);
      let mut built = self.build_decl(&*i)?;
      program.append(&mut built);
    }
    
    program.push(u(END));

    let mut file = File::create("foo.ops").unwrap();
    file.write_all(&program).unwrap();

    let program: Program = program.iter().map(|c| Operation {
      code: OPCode::from_i32(*c as i32),
      val: *c
    }).collect();

    Ok(program)
  }
}

pub struct VM {
  vm_exec: VMExec,
  vm_build: VMBuild
}

impl VM {
  pub fn new() -> Self {
    // let op: Operation = Operation::POP;
    // println!("{:?}, {}", Operation::from_i32(0x2), op as i32);

    Self {
      vm_exec: VMExec::new(),
      vm_build: VMBuild::new()
    }
  }

  pub fn build(&mut self, decls: Decls, code: String, options: BuildOptions) -> Result<Program, VMBuildError> {
    self.vm_build.build(decls, code, options)
  }

  pub fn exec(&mut self, program: Program) -> Result<String, VMExecError> {
    self.vm_exec.exec(program)
  }

  

}