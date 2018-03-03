use LangErr;
use lexer::LexErr;
use parser::ParserErr;

fn print_err(title: &str, err_pos: i32, description: &str, query: &str) {
  print!("{}\n{}", title, query);

  if err_pos >= 0 {
    let mut offset = String::from("");
    for _i in 0..err_pos { offset.push('-') }
    println!("{}^", offset);
  }

  if description != "" {
    let mut offset = String::from("");
    for _i in 0..err_pos { offset.push(' ') }
    println!("{}{}", offset, description);
  }
}

fn lexer_err(err: &LexErr, query: &str) {
  let mut title = "";
  let mut err_pos = -1; // if -1 then not valid
  let mut description = String::from("");

  match err {
    &LexErr::MismatchedQuotes(pos) => {
      title = "Lexer error: MismatchedQuotes";
      err_pos = pos;
      description = String::from("mismatched quote");
    },
    &LexErr::UnknownToken(pos, token) => {
      title = "Lexer error: UnknownToken";
      err_pos = pos;
      description = format!("unknown token \"{}\"", token);
    },
    _ => {
      title = "Lexer error: undefined"
    }
  }

  print_err(title, err_pos, &description, query);
}

fn parser_err(err: &ParserErr, query: &str) {
  let mut title = "";
  let mut err_pos = -1;
  let mut description = String::from("");

  match err {
    &ParserErr::UnexpectedIdentifier(pos) => {
      title = "Parser error: UnexpectedIdentifier";
      err_pos = pos;
      description = format!("unexpected identifier");
    },
    &ParserErr::UnexpectedLiteral(pos) => {
      title = "Parser error: UnexpectedLiteral";
      err_pos = pos;
      description = format!("unexpected literal");
    },
    &ParserErr::UnexpectedToken(pos, token, ref _allowed_tokens, ref _allowed_literal, _allowed_identifier) => {
      title = "Parser error: UnexpectedToken";
      err_pos = pos;
      description = format!("unexpected token: \"{:#?}\"", token);
    },
    &ParserErr::UnexpectedEndOfLine(pos) => {
      title = "Parser error: UnexpectedEndOfLine";
      err_pos = pos;
      description = format!("unexpected end of line");
    },
    _ => {
      title = "Parser error: undefined";
    }
  }

  print_err(title, err_pos, &description, query);
}

pub fn handle_err(err: &LangErr, query: &str) {
  match err {
    &LangErr::LexErr(ref err) => lexer_err(err, query),
    &LangErr::ParserErr(ref err) => parser_err(err, query),
    _ => { println!("Unhandled error!"); }
  }
}