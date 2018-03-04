extern crate ansi_term;

use LangErr;
use lexer::LexErr;
use parser::ParserErr;
use interpreter::InterpreterErr;

fn print_err(title: &str, err_pos: i32, description: &str, query: &str) {
  let query_vec: Vec<&str> = query.split('\n').collect();

  let mut line = 0;
  for (k, v) in query.chars().enumerate() {
    if k >= err_pos as usize {
      break;
    }
    if v == '\n' {
      line += 1;
    }
  }

  let pre_query = if line >= 1 { format!("{}\n", query_vec[line - 1]) } else { format!("") };
  let query = query_vec[line];
  let post_query = if line <= query_vec.len() - 1  { format!("{}\n", query_vec[line + 1]) } else { format!("") };
  print!("{}\n{}{}\n", title, pre_query, query);

  let mut err_pos = err_pos - 1;
  for i in 0..line {
    err_pos -= query_vec[i].len() as i32;
  }
 
  if err_pos >= 0 {
    let mut offset = String::from("");
    for _i in 0..err_pos { offset.push('-') }
    println!("{}{}", ansi_term::Color::Red.bold().paint(offset), ansi_term::Color::Red.bold().paint("^"));
  }

  if description != "" {
    let mut offset = String::from("");
    for _i in 0..err_pos { offset.push(' ') }
    println!("{}{}", offset, ansi_term::Color::Red.bold().paint(description));
  }

  println!("{}", post_query);
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

fn interpreter_err(err: &InterpreterErr, query: &str) {
  let mut title = "";
  let mut err_pos = -1;
  let mut description = String::from("");

  match err {
    &InterpreterErr::ArithmeticErr(ref left, ref right, ref operation, ref pos) => {
      title = "Interpreter error: ArithmeticErr";
      err_pos = *pos;
      description = format!("opereration \"{:?}\" not defined for types: \"{}\", \"{}\"", operation, left, right);
    },
    &InterpreterErr::IdentifierNotFound(ref identifier, ref pos) => {
      title = "Interpreter error: IdentifierNotFound";
      err_pos = *pos;
      description = format!("identifier \"{}\" not found in this scope", identifier);
    },
    _ => {
      title = "Unknown interpreter error!"
    }
  }

  print_err(title, err_pos, &description, query);
}

pub fn handle_err(err: &LangErr, query: &str) {
  match err {
    &LangErr::LexErr(ref err) => lexer_err(err, query),
    &LangErr::ParserErr(ref err) => parser_err(err, query),
    &LangErr::InterpreterErr(ref err) => interpreter_err(err, query),
  }
}