extern crate num;
use self::num::FromPrimitive;

use parser::Declaration;

pub use super::lexer;
pub use lexer::Token;
// pub use lexer::Literal;

pub mod build;
pub mod exec;

use vm::build::VMBuild;
use vm::exec::VMExec;
use vm::exec::Value;

use self::OPCode::*;

pub use vm::build::VMBuildError;
pub use vm::build::VMBuildError::*;
pub use vm::exec::VMExecError;
pub use vm::exec::VMExecError::*;

use std::slice;
use std::mem;
use std::fs::File;
use std::io::prelude::*;

pub type Program = Vec<Operation>;
pub type Decls = Vec<Box<Declaration>>;

const NIL: *const Value = &Value::NIL;

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