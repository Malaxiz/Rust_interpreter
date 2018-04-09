extern crate num;
use self::num::FromPrimitive;

pub use super::parser::Declaration;

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

#[derive(Debug)]
pub enum VMBuildError {

}

#[derive(Debug)]
pub enum VMExecError {
  ArithmeticErr(String),
  InvalidOPCode(String)
}

#[derive(Debug)]
pub enum VMError {
  VMBuildError(VMBuildError),
  VMExecError(VMExecError),
  Temp
}

enum_from_primitive! {
  #[derive(Debug, PartialEq)]
  pub enum OPCode {
    END = 0x01,
    VERSION,
    INFO,
    META_END,

    PUSH_INT,
    POP,
    PEEK,
    ADD,
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
  val: u8
  // description: Option<>
}

#[derive(Copy, Clone, Debug)]
enum Literal {
  Int(i32),

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

pub struct VM {
  root: Root,
  stack: [*const Value; 512],
  stacki: usize
}

impl VM {
  pub fn new() -> Self {
    // let op: Operation = Operation::POP;
    // println!("{:?}, {}", Operation::from_i32(0x2), op as i32);

    Self {
      root: Root::new(),
      stack: [&Value::None; 512],
      stacki: 0
    }
  }

  pub fn build(&mut self, decls: Decls, options: BuildOptions) -> Result<Program, VMError> {
    let program: Vec<u8> = vec![VERSION as u8, 0x01, META_END as u8,
                                PUSH_INT as u8, 0x0f, PUSH_INT as u8, 0xf0, ADD as u8, END as u8];

    let mut file = File::create("foo.ops").unwrap();
    file.write_all(&program).unwrap();

    let program: Program = program.iter().map(|c| Operation {
      code: OPCode::from_i32(*c as i32),
      val: *c
    }).collect();

    Ok(program)
  }

  pub fn exec(&mut self, program: Program) -> Result<String, VMError> {
    let mut program = vec![];
    let mut f = File::open("foo.ops").unwrap();
    for byte in f.bytes() {
      program.push(byte.unwrap());
    }

    let program: Program = program.iter().map(|c| Operation {
      code: OPCode::from_i32(*c as i32),
      val: *c
    }).collect();

    println!("{:?}", program);

    let mut op_i = 0;
    let mut meta_end = false;
    loop {
      let op = &program[op_i];
      let code = &op.code;
      if !meta_end {
        if code == &Some(OPCode::META_END) {
          meta_end = true;
        }

        // meta things
        op_i += 1;
        continue;
      }

      println!("{:?}", op);

      if let &Some(ref code) = code {

        print!("stack: {}\n---------\n", self.stacki);
        for (i, v) in self.stack.iter().enumerate() {
          unsafe {
            if *v != &Value::None {
              print!("    {}: {:?}\n", i, **v);
            }
          }
        }
        print!("---------\n");
        
        match *code {
          END => {
            unsafe {
              return Ok(format!("{:?}", *self.stack_pop()));
            }
          },
          PUSH_INT => {
            op_i += 1;
            let int: i32 = program[op_i].val as i32;
            let val = Box::new(Value::Literal(Literal::Int(int)));
            let val_point = &*val as *const Value;
            self.root.pool.push(val);
            self.stack_push(val_point);
          },
          ADD => {
            let second = self.stack_pop();
            let first = self.stack_pop();
            let res = self.literal_operation(first, second, code)?;
            self.stack_push(res);
            unsafe {
              println!("{:?} + {:?}", *first, *second);
            }
          },
          POP => {
            self.stack_pop();
          }
          _ => ()
        };

        op_i += 1;
      } else {
        return Err(VMError::Temp)
      }
    }
  }

  fn stack_push(&mut self, val: *const Value) {
    self.stack[self.stacki] = val;
    self.stacki += 1;
  }

  fn stack_pop(&mut self) -> *const Value {
    self.stacki -= 1;
    self.stack[self.stacki]
  }

  fn literal_operation(&mut self, val1: *const Value, val2: *const Value, operation: &OPCode) -> Result<*const Value, VMError> {
    
    unsafe {
      match (*val1, *val2) {
        (Value::Literal(val1), Value::Literal(val2)) => {
          let res: Value = match (val1, val2, operation) {
            (Literal::Int(first), Literal::Int(second), &ADD) => {
              Value::Literal(Literal::Int(first + second))
            },
            _ => return Err(VMError::VMExecError(ArithmeticErr(String::from("Operation not supported"))))
          };

          let res = Box::new(res);
          let res_point: *const Value = &*res;
          self.root.pool.push(res);
          Ok(res_point)
        },
        _ => { Err(VMError::Temp) }
      }
    }
  }

}