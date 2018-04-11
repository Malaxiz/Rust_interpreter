#[macro_use] extern crate enum_primitive;
#[macro_use] extern crate bitflags;

pub mod lexer;
pub mod parser;
pub mod interpreter;
pub mod vm;

use interpreter::Interpreter;
use vm::VM;
use vm::BuildOptions;
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

fn do_exec(query: &str, interpreter: &mut VM) -> Result<String, LangErr> {
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

  let built = match interpreter.build(parsed, String::from(query), BuildOptions::DEBUG) {
    Ok(val) => val,
    Err(err) => return Err(LangErr::VMBuildErr(err))
  };

  let executed = match interpreter.exec(built) {
    Ok(val) => val,
    Err(err) => return Err(LangErr::VMExecErr(err))
  };

  // let interpreted = match interpreter.exec(&parsed) {
  //   Ok(val) => {
  //     match val {
  //       Literal::Bool(b) => String::from(if b { "true" } else { "false" }),
  //       Literal::String(s) => s,
  //       Literal::Num(n) => n.to_string(),
  //       Literal::Nil => String::from("nil"),
  //       Literal::Function(parameters, body) => format!("<function ({:?})>", parameters),

  //       Literal::Variable(v) => String::from("variable")
  //     }
  //   },
  //   Err(err) => return Err(LangErr::InterpreterErr(err))
  // };

  Ok(executed)
}

pub fn exec(query: &str, interpreter: &mut VM) -> Result<String, LangErr> {
  let result = do_exec(query, interpreter);
  match result {
    Err(ref err) => {
      // println!("{:?}", err);
      handle_err::handle_err(err, query)
    },
    _ => {}
  };

  result
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
