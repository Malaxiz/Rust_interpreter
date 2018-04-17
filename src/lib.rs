#[macro_use] extern crate enum_primitive;
#[macro_use] extern crate bitflags;

pub mod lexer;
pub mod parser;
pub mod interpreter;
pub mod vm;

use std::fs::File;
use std::io::prelude::*;

use interpreter::Interpreter;
use vm::{VM, BuildOptions, Program, Instructions};
use lexer::Literal;

mod handle_err;

#[derive(Debug)]
pub enum LangErr {
  LexErr(lexer::LexErr),
  ParserErr(parser::ParserErr),
  InterpreterErr(interpreter::InterpreterErr),
  VMExecErr(vm::VMExecError),
  VMBuildErr(vm::VMBuildError)
}

fn do_build(query: &str, vm: &mut VM, options: BuildOptions) -> Result<Instructions, LangErr> {
  let lexed = match lexer::lex(query) {
    Ok(val) => val,
    Err(err) => return Err(LangErr::LexErr(err))
  };

  let parsed = match parser::parse(lexed) {
    Ok(val) => val,
    Err(err) => return Err(LangErr::ParserErr(err))
  };

  let instructions = match vm.build(parsed, String::from(query), options) {
    Ok(val) => val,
    Err(err) => return Err(LangErr::VMBuildErr(err))
  };

  Ok(instructions)
}

pub fn build(query: &str, vm: &mut VM, options: BuildOptions) -> Result<Instructions, LangErr> {
  match do_build(query, vm, options) {
    Ok(val) => Ok(val),
    Err(err) => {
      handle_err::handle_err(&err, query);
      Err(err)
    }
  }
}

pub fn exec(program: Program, vm: &mut VM, append: bool) -> Result<String, LangErr> {
  match vm.exec(program, append) {
    Ok(val) => Ok(val),
    Err(err) => {
      let err = LangErr::VMExecErr(err);
      if vm.vm_exec.is_debug {
        handle_err::handle_err(&err, &vm.vm_exec.query);
      } else {
        handle_err::print_err_text(&format!("{:?}\n", err));
      }
      Err(err)
    }
  }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
