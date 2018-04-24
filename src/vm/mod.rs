extern crate num;
use self::num::FromPrimitive;
use std::str;

use parser::Declaration;

pub use super::lexer;
pub use lexer::Token;

pub mod build;
pub mod exec;
pub mod cast;
pub mod native;

use vm::build::VMBuild;
use vm::exec::{VMExec, Value};

use self::OPCode::*;

pub use vm::build::{VMBuildError, VMBuildError::*};
pub use vm::exec::{VMExecError, VMExecError::*};

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

    VERSION, // [version: 1b]
    DEBUG, // []

    META_END,

    DEBUG_CODE,
    DEBUG_CODE_END,

    I32, // [content: 4b],
    STRING, // [content: str, NULL]

    PUSH_NUM, // [content: 8b]
    PUSH_INT, // [content: 4b]
    PUSH_JUMP, // [content: 4b], position for a JUMPSTACK to use, gets pushed to a separate stack
    PUSH_BOOL,  // [content: 1b]
    PUSH_STRING, // [content: str, NULL]
    PUSH_VAR, // [content: str, NULL, I32, debug: 4b], // looks up variable
    PUSH_VALUE, // [], pops a value, checks if variable, then pushes the value of the variable 
    PUSH_VALUE_DIRECT, // [], pops a value, checks if variable directly, then pushes the value of the variable 
    PUSH_POINTER, // [] // pops a value, pushes a pointer to the value in its scope
    // PUSH_STACK_VAR, // [I32, debug: 4b] //  pops a string from the stack and looks up variable
    PUSH_FUNC, // [I32, debug: 4b, I32, pos: 4b, I32, parameter_len: 4b, STRING, par1: str, STRING, par2: str, ..., body_len: 4b, body]
    CALL_FUNC, // [I32, debug: 4b, I32, argument_len: 4b],
    CALL_FUNC_STACK_ARGS, // [I32, debug: 4b] // same as CALL_FUNC, but with the argument length on the stack
    PUSH_STRUCT, // [I32, debug: 4b, I32, pos: 4b],
    CALL_STRUCT,
    GET_SCOPE, // [I32, debug: 4b]
    PUSH_NIL, // []

    POP, // pops top value of stack

    // [content: 0b]
    SCOPE_NEW,
    SCOPE_NEW_FUNC, // peeks at a function and checks its scope
    SCOPE_END,
    SCOPE_FORWARD, // moves scope_stacki += 1
    SCOPE_BACK, // moves scope_stacki -= 1
    SCOPE_PUSH, // for instances, not implemented

    JUMP, // [I32, pos: 4b]
    JUMPIFN, // [I32, debug: 4b, I32, pos: 4b]
    JUMPSTACK, // [], jumps to a relative position
    JUMPSTACKABS, // [], jumps to an absolute position

    // operation on top two stack values. [I32, debug: 4b]
    ASSIGN,
    LET,
    ADD,
    SUB,
    MULTIPLY,
    DIVIDE,
    DOT,

    // [I32, debug: 4b]
    LT,
    GT,
    LTOREQ,
    GTOREQ,
  }
}

bitflags! {
  pub struct BuildOptions: u32 {
    const NONE = 0x00;
    const DEBUG = 0x01;
    const PROGRESS = 0x02;
    const CODE = 0x03;
  }
}

// bitflags! {
//   pub struct ExecOptions: u32 {
//     const NONE = 0x00;
//     const STRICT_FUNCTIONS = 0x01;
//     const STRICT_ASSIGNMENT = 0x02;
//   }
// }

#[derive(Debug)]
pub enum OperationLiteral {
  Num(f64),
  String(String, usize),
  Int(i32),

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
  let blen = bytes.len();
  while(i < blen) {
    let op = bytes[i];
    let code = OPCode::from_i32(op as i32);

    let mut content = unsafe {
      match code {
        Some(val) => match val {
          PUSH_NUM => {
            if i + 8 < blen {
              let mut content_vec: [u8; 8] = [0x00; 8];
              for j in 0..8 {
                let op = bytes[i+j+1];
                content_vec[j] = op;
              }
              OperationLiteral::Num(mem::transmute::<[u8; 8], f64>(content_vec)) 
            } else {
              OperationLiteral::None
            }
          },
          STRING | PUSH_STRING | PUSH_VAR => {
            let mut content_vec: Vec<u8> = Vec::new();
            let mut j = 0;
            //let content = OperationLiteral::None;
            let mut is_invalid = false;
            loop {
              if i + j + 1 > blen { // invalid
                is_invalid = true;
                break;
              }
              let op = bytes[i+j+1];
              if op == u(NULL) {
                break;
              }
              content_vec.push(op);
              j += 1;
            }

            if !is_invalid {
              let s = String::from(match str::from_utf8(&content_vec) {
                Ok(v) => v,
                Err(e) => panic!("Invalid UTF-8 sequence: {}", e),
              });
              let len = s.len() + 1; // + 1 null terminator
              OperationLiteral::String(s, len)
            } else {
              OperationLiteral::None
            }
          },
          I32 | PUSH_INT | PUSH_JUMP => {
            if i + 4 < blen {
              let mut content_vec: [u8; 4] = [0x00; 4];
              for j in 0..4 {
                let op = bytes[i+j+1];
                content_vec[j] = op;
              }
              OperationLiteral::Int(mem::transmute::<[u8; 4], i32>(content_vec))
            } else {
              OperationLiteral::None
            }
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
  pub vm_exec: VMExec,
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

  pub fn get_query(self) -> String {
    self.vm_exec.query
  }

  pub fn build(&mut self, decls: Decls, code: String, debug_offset: usize, options: BuildOptions) -> Result<Instructions, VMBuildError> {
    self.vm_build.build(decls, code, debug_offset, options)
  }

  pub fn exec(&mut self, mut program: Program, append: bool) -> Result<String, VMExecError> {
    self.vm_exec.exec(program, append)
  }
}