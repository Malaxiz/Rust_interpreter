#[macro_use] extern crate enum_primitive;
#[macro_use] extern crate bitflags;

pub mod lexer;
pub mod parser;
pub mod vm;
pub mod interpreter;

// use interpreter::Interpreter;
use lexer::Literal;

use vm::VM;
use vm::BuildOptions;

mod handle_err;

#[derive(Debug)]
pub enum LangErr {
  LexErr(lexer::LexErr),
  ParserErr(parser::ParserErr),
  // InterpreterErr(interpreter::InterpreterErr),
  VMError(vm::VMError)
}

fn do_exec(query: &str, vm: &mut VM) -> Result<String, LangErr> {
  let lexed = match lexer::lex(query) {
    Ok(val) => val,
    Err(err) => return Err(LangErr::LexErr(err))
  };
  // println!("{:?}", lexed);

  let parsed = match parser::parse(lexed) {
    Ok(val) => val,
    Err(err) => return Err(LangErr::ParserErr(err))
  };
  // println!("{:?}", parsed);

  let program: vm::Program = match vm.build(parsed, BuildOptions::DEBUG | BuildOptions::PROGRESS) {
    Ok(val) => val,
    Err(err) => return Err(LangErr::VMError(err))
  };

  let res = match vm.exec(program) {
    Ok(val) => val,
    Err(err) => return Err(LangErr::VMError(err))
    // Ok(val) => {
    //   match val {
    //     Literal::Bool(b) => String::from(if b { "true" } else { "false" }),
    //     Literal::String(s) => s,
    //     Literal::Num(n) => n.to_string(),
    //     Literal::Nil => String::from("nil"),
    //     Literal::Function(parameters, body) => format!("<function ({:?})>", parameters),

    //     Literal::Variable(v) => String::from("variable")
    //   }
    // },
    // Err(err) => return Err(LangErr::InterpreterErr(err))
  };

  Ok(res)
}

pub fn exec(query: &str, vm: &mut VM) -> Result<String, LangErr> {
  let result = do_exec(query, vm);
  match result {
    Err(ref err) => {
      // println!("{:?}", err);
      handle_err::handle_err(err, query)
    },
    _ => {}
  };

  result
}

pub fn exec_program(program: &str, vm: &mut VM) {
  
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
