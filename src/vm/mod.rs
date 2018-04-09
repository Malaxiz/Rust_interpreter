extern crate num;
use self::num::FromPrimitive;

pub use super::parser::Declaration;

use std::str;
use std::slice;
use std::mem;

use std::fs::File;
use std::io::prelude::*;

pub type Program = Vec<Operation>;
pub type Decls = Vec<Box<Declaration>>;

#[derive(Debug)]
pub enum VMBuildError {

}

#[derive(Debug)]
pub enum VMExecError {

}

#[derive(Debug)]
pub enum VMError {
  VMBuildError(VMBuildError),
  VMExecError(VMExecError)
}

enum_from_primitive! {
  #[derive(Debug, PartialEq)]
  pub enum OPCode {
    END = 0x01,
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

pub struct VM {

}

impl VM {
  pub fn new() -> Self {
    // let op: Operation = Operation::POP;
    // println!("{:?}, {}", Operation::from_i32(0x2), op as i32);

    Self {

    }
  }

  pub fn build(&mut self, decls: Decls, options: BuildOptions) -> Result<Program, VMError> {
    
    let program: Vec<u8> = vec![OPCode::PUSH_INT as u8, 0x0f, OPCode::PUSH_INT as u8, 0xf0, OPCode::ADD as u8, OPCode::END as u8];
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
    loop {
      let op = &program[op_i];
      let code = &op.code;
      if code == &Some(OPCode::END) {
        break;
      }

      println!("{:?}", op);

      op_i += 1;
    }

    Ok(String::from(""))
  }

}