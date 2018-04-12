extern crate num;
use self::num::FromPrimitive;
use std::str;

use parser::Declaration;

pub use super::lexer;
pub use lexer::Token;

pub mod build;
pub mod exec;

use vm::build::VMBuild;
use vm::exec::{VMExec, Value};

use self::OPCode::*;

pub use vm::build::{VMBuildError, VMBuildError::*};
pub use vm::exec::{VMExecError, VMExecError::*};

use std::slice;
use std::mem;

pub type Program = Vec<Operation>;
pub type Instructions = Vec<u8>;
pub type Decls = Vec<Box<Declaration>>;

const NIL: *const Value = &Value::Literal(exec::Literal::Nil);

pub fn u(op_code: OPCode) -> u8 {
  op_code as u8
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

    PUSH_NUM,
    PUSH_BOOL,
    PUSH_STRING,
    PUSH_VAR,

    POP,
    PEEK,

    JUMP,
    JUMPIF,

    // operation on top two stack values.
    ASSIGN,
    ADD,
    SUB,
    MULTIPLY,

    LT,
    GT,
    LTOREQ,
    GTOREQ,
  }
}

bitflags! {
  pub struct BuildOptions: u32 {
    const DEBUG = 0x01;
    const PROGRESS = 0x02;
  }
}

#[derive(Debug)]
pub enum OperationLiteral {
  Num(f64),
  String(String, usize),

  None
}

#[derive(Debug)]
pub struct Operation {
  code: Option<OPCode>,
  val: u8,
  content: OperationLiteral
}

pub fn get_program(bytes: Vec<u8>) -> Program {
  let mut program = Vec::new();
  let mut i = 0;
  while(i < bytes.len()) {
    let op = bytes[i];

    let code = OPCode::from_i32(op as i32);
    let mut content = unsafe {
      match code {
        Some(val) => match val {
          PUSH_NUM => {
            let mut content_vec: [u8; 8] = [0x00; 8];
            for j in 0..8 {
              let op = bytes[i+j+1];
              content_vec[j] = op;
            }
            OperationLiteral::Num(mem::transmute::<[u8; 8], f64>(content_vec))
          },
          PUSH_STRING => {
            let mut content_vec: Vec<u8> = Vec::new();
            let mut j = 0;
            loop {
              let op = bytes[i+j+1];
              if op == u(NULL) {
                break;
              }
              content_vec.push(op);
              j += 1;
            }
            let s = String::from(match str::from_utf8(&content_vec) {
              Ok(v) => v,
              Err(e) => panic!("Invalid UTF-8 sequence: {}", e),
            });
            let len = s.len() + 1; // + 1 null terminator
            OperationLiteral::String(s, len)
          },
          _ => OperationLiteral::None
        },
        None => OperationLiteral::None
      }
    };

    program.push(Operation {
      code,
      val: op,
      content
    });
    i += 1;
  }

  program
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

  pub fn build(&mut self, decls: Decls, code: String, options: BuildOptions) -> Result<Instructions, VMBuildError> {
    self.vm_build.build(decls, code, options)
  }

  pub fn exec(&mut self, program: Program) -> Result<String, VMExecError> {
    self.vm_exec.exec(program)
  }
}