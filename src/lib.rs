pub mod lexer;
pub mod parser;

mod handle_err;

pub enum LangErr<'a> {
  LexErr(lexer::LexErr<'a>),
  ParserErr(parser::ParserErr<'a>),
}

fn do_exec(query: &str) -> Result<String, LangErr> {
  let lexed = match lexer::lex(query) {
    Ok(val) => val,
    Err(err) => return Err(LangErr::LexErr(err))
  };

  let parsed = match parser::parse(&lexed) {
    Ok(val) => val,
    Err(err) => return Err(LangErr::ParserErr(err))
  };

  Ok(String::from("temp"))
}

pub fn exec(query: &str) -> Result<String, LangErr> {
  let result = do_exec(query);
  match result {
    Err(ref err) => handle_err::handle_err(err, query),
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
