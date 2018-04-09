extern crate ansi_term;

use LangErr;
use lexer::LexErr;
use parser::ParserErr;
use vm::VMError;

fn print_err(title: &str, err_pos: i32, width: i32, description: &str, query: &str) {
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

  let pre_query = if line >= 1 { format!("| {}\n", query_vec[line - 1]) } else { format!("") };
  let query = query_vec[line];
  let post_query = if line < query_vec.len() - 1  { format!("| {}\n", query_vec[line + 1]) } else { format!("") };
  print!("{}\n{}| {}\n", ansi_term::Color::Cyan.bold().paint(title), pre_query, query);

  let mut err_pos = err_pos;
  for i in 0..line {
    err_pos -= query_vec[i].len() as i32 + 1;
  }
 
  if err_pos >= 0 {
    let mut offset = String::from("");
    for _i in 0..err_pos { offset.push('-') }
    println!("| {}{}", ansi_term::Color::Red.bold().paint(offset), ansi_term::Color::Red.bold().paint("^".repeat(width as usize)));
  }

  if description != "" {
    let mut offset = String::from("");
    for _i in 0..err_pos { offset.push(' ') }
    print!("| {}{}\n", offset, ansi_term::Color::Red.bold().paint(description));
  }

  print!("{}", post_query);
}

fn lexer_err(err: &LexErr, query: &str) {
  let mut title = "";
  let mut err_pos = -1; // if -1 then not valid
  let mut width = 1;
  let mut description = String::from("");

  match err {
    &LexErr::MismatchedQuotes(pos) => {
      title = "Lexer error: MismatchedQuotes";
      err_pos = pos;
      description = String::from("mismatched quote");
    },
    &LexErr::UnknownToken(ref token, pos) => {
      title = "Lexer error: UnknownToken";
      err_pos = pos;
      description = format!("unknown token \"{}\"", token);
    },
    &LexErr::UnknownEscapeSequence(c, pos) => {
      title = "Lexer error: UnknownEscapeSequence";
      err_pos = pos - 1;
      width = 2;
      description = format!("unknown escape sequence: \"{}\"", c);
    }
    _ => {
      title = "Lexer error!";
      description = format!("{:?}", err);
    }
  }

  print_err(title, err_pos, width, &description, query);
}

fn parser_err(err: &ParserErr, query: &str) {
  let mut title = "";
  let mut err_pos = -1;
  let mut width = 1;
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
    &ParserErr::ExpectedSemiColon(pos) => {
      title = "Parser error: ExpectedSemiColon";
      err_pos = pos;
      description = format!("expected semicolon");
    },
    &ParserErr::ExpectedBraceOpen(pos) => {
      title = "Parser error: ExpectedBraceOpen";
      err_pos = pos;
      description = format!("expected open brace for this expression");
    },
    &ParserErr::MismatchedParenthesis(pos) => {
      title = "Parser error: MismatchedParenthesis";
      err_pos = pos;
      description = format!("mismatched parenthesis");
    },
    // &ParserErr::ExpectedArrow(pos) => {
    //   title = "Parser error: ExpectedArrow";
    //   err_pos = pos;
    //   description = format!("expected arrow for function expression");
    // },
    &ParserErr::ExpectedIdentifier(pos) => {
      title = "Parser error: ExpectedIdentifier";
      err_pos = pos;
      description = format!("expected identifier");
    }
    _ => {
      title = "Parser error!";
      description = format!("{:?}", err);
    }
  }

  print_err(title, err_pos, width, &description, query);
}

// fn interpreter_err(err: &InterpreterErr, query: &str) {
//   let mut title = "";
//   let mut err_pos = -1;
//   let mut width = 1;
//   let mut description = String::from("");

//   match err {
//     &InterpreterErr::ArithmeticErr(ref left, ref right, ref operation, ref pos) => {
//       title = "Interpreter error: ArithmeticErr";
//       err_pos = *pos;
//       description = format!("opereration \"{:?}\" not defined for types: \"{}\", \"{}\"", operation, left, right);
//     },
//     &InterpreterErr::IdentifierNotFound(ref identifier, ref pos) => {
//       title = "Interpreter error: IdentifierNotFound";
//       err_pos = *pos;
//       description = format!("identifier \"{}\" not found in this scope", identifier);
//     },
//     &InterpreterErr::CastErr(ref type1, ref type2, ref pos) => {
//       title = "Interpreter error: CastErr";
//       err_pos = *pos;
//       description = format!("cannot cast type \"{}\" into \"{}\"", type1, type2);
//     }
//     _ => {
//       title = "Interpreter error!";
//       description = format!("{:?}", err);
//     }
//   }

//   print_err(title, err_pos, width, &description, query);
// }

fn vm_err(err: &VMError, query: &str) {

}

pub fn handle_err(err: &LangErr, query: &str) {
  match err {
    &LangErr::LexErr(ref err) => lexer_err(err, query),
    &LangErr::ParserErr(ref err) => parser_err(err, query),
    // &LangErr::InterpreterErr(ref err) => interpreter_err(err, query),
    &LangErr::VMError(ref err) => vm_err(err, query)
  }
}