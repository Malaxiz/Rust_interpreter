pub mod lexer;
pub mod parser;
pub mod interpreter;

use interpreter::Interpreter;
use lexer::Literal;

mod handle_err;

#[derive(Debug)]
pub enum LangErr<'a> {
  LexErr(lexer::LexErr<'a>),
  ParserErr(parser::ParserErr),
  InterpreterErr(interpreter::InterpreterErr)
}

fn do_exec<'a>(query: &'a str, interpreter: &'a mut Interpreter) -> Result<String, LangErr<'a>> {
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

  let interpreted = match interpreter.exec_program(&parsed) {
    Ok(val) => {
      match val {
        Literal::Bool(b) => String::from(if b { "true" } else { "false" }),
        Literal::String(s) => s,
        Literal::Num(n) => n.to_string(),
        Literal::Nil => String::from("nil"),
        Literal::Variable(v) => String::from("variable")
      }
    },
    Err(err) => return Err(LangErr::InterpreterErr(err))
  };

  Ok(interpreted)
}

pub fn exec<'a>(query: &'a str, interpreter: &'a mut Interpreter) -> Result<String, LangErr<'a>> {
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
