//! The lexer module function `lex` takes a `query: &str` and lexes it into a `Vec<Lexed>` which can then be parsed.

mod info;

pub use self::info::Token;
pub use self::info::LexErr;
pub use self::info::get_tokens;

use std::collections::HashMap;

#[derive(Debug)]
enum PreLexed<'a> {
  String(&'a str, i32),
  Rest(&'a str, i32),
}

#[derive(Debug)]
pub enum Literal {
  String(String),
  Num(f64),
}

#[derive(Debug)]
pub enum Lexed<'a> {
  Literal(Literal, i32),
  Operator(Token, i32),
  Identifier(&'a str, i32),
}

fn pre_lex(query: &str) -> Result<Vec<PreLexed>, LexErr> {
  let mut pre_lexed: Vec<PreLexed> = Vec::new();

  let mut quote_count = 0;
  let mut in_quote = false;
  let mut start = 0;
  let mut skip_next = false;

  for (i, c) in query.chars().enumerate() {
    match c {
      '\\' => {
        skip_next = true;
        continue;
      },
      '"' => {
        if skip_next {
          continue;
        }

        quote_count += 1;
        in_quote = !in_quote;

        let s = &query[start..i];
        pre_lexed.push(if in_quote {
          PreLexed::Rest(s, start as i32)
        } else {
          PreLexed::String(s, start as i32)
        });

        start = i+1;
      },
      _ => { }
    }
    skip_next = false;
  }

  if quote_count % 2 != 0 {
    return Err(LexErr::MismatchedQuotes((start - 1) as i32));
  }

  pre_lexed.push(PreLexed::Rest(&query[start..query.len()], start as i32));

  Ok(pre_lexed)
}

fn is_operator(tokens: &Vec<(&& str, &Token)>, trimmed: &str) -> Option<Token> {
  for &i in tokens.iter() {
    if *i.0 == trimmed {
      return Some(i.1.clone());
    }
  }
  None
}

fn is_identifier(val: &str) -> bool {
  if val.len() <= 0 { return false; }

  match val.chars().next().unwrap() {
    'a' ... 'z' | 'A' ... 'Z' | '_' => { },
    _ => { return false; }
  }

  for i in val.chars() {
    match i {
      'a' ... 'z' | 'A' ... 'Z' | '_' | '0' ... '9' => { },
      _ => { return false; }
    }
  }

  true
}

fn is_number(val: &str) -> bool {
  if val.len() <= 0 { return false; }

  let mut num_dots = 0;
  let mut num_e = 0;

  let mut prev_dot = false;
  let mut prev_e = false;

  match val.chars().next().unwrap() {
    '.' | 'e' => { return false; }
    _ => {}
  }

  for i in val.chars() {
    match i {
      '0' ... '9' => {
        prev_dot = false;
        prev_e = false;
      },
      '.' => {
        if num_dots >= 1 || prev_e {
          return false;
        }
        num_dots += 1;
        prev_dot = true;
      },
      'e' => {
        if num_e >= 1 || prev_dot {
          return false;
        }
        num_e += 1;
        prev_e = true;
      }
      _ => { return false; }
    }
  }

  if prev_dot || prev_e {
    return false;
  }

  true
}

/// Trims whitespace
/// .1 = start, .2 = end
fn trim<'a>(val: &'a str) -> (&'a str, i32, i32) {
  if val.len() <= 0 {
    return (val, 0, 0);
  }

  let mut start: i32 = 0;
  let mut end: i32 = val.len() as i32 - 1;

  for (k, v) in val.chars().enumerate() {
    match v {
      ' ' => {}
      _ => {
        start = k as i32;
        break;
      }
    }
  };

  for (k, v) in val.chars().rev().enumerate() {
    match v {
      ' ' => {}
      _ => {
        end -= k as i32 - 1;
        break;
      }
    }
  };

  (&val[start as usize..end as usize], start, end)
}

fn tokenize(pre_lexed: Vec<PreLexed>) -> Result<Vec<Lexed>, LexErr> {

  let tokens: HashMap<&str, Token> = info::get_tokens();

  let tokens: Vec<(&&str, &Token)> = tokens.iter().collect();
  // tokens.sort_by(|a, b| b.0.len().cmp(&a.0.len()));

  let mut lexed: Vec<Lexed> = Vec::new();
  for i in pre_lexed {
    match i {
      PreLexed::Rest(val, pos) => {
        let mut offset = 0;
        let mut r_offset: i32 = -1;
        let len = val.len();

        while offset < len {
          r_offset += 1;

          if (len - r_offset as usize) - offset <= 0 {
            break;
          }

          let curr = &val[offset..len - r_offset as usize];
          let trimmed = trim(curr);
          let apos = pos + trimmed.1 + offset as i32;
          // println!("curr: {}, trim: {}, off: {}, r_off: {}, lex: {}", curr, trimmed.0, offset, r_offset, lexed.len());

          if curr.len() <= 0 {
            break;
          } else if curr.len() == 1 && curr.chars().next().unwrap() == ' ' {
            break;
          }

          if let Some(op) = is_operator(&tokens, trimmed.0) {
            lexed.push(Lexed::Operator(op, apos));
          } else if is_identifier(trimmed.0) {
            lexed.push(Lexed::Identifier(trimmed.0, apos));
          } else if is_number(trimmed.0) {
            lexed.push(Lexed::Literal(Literal::Num(trimmed.0.parse::<f64>().unwrap()), apos));
          } else if trimmed.0.len() <= 1 {
            return Err(LexErr::UnknownToken(apos, trimmed.0));
          } else {
            continue;
          }

          r_offset = -1;
          offset += trimmed.0.len() + trimmed.1 as usize;
        }
      },
      PreLexed::String(val, pos) => {
        lexed.push(Lexed::Literal(Literal::String(String::from(val)), pos));
      }
    };
  }

  Ok(lexed)
}

/// Lexes a `query: &str` into a vector of tokens: `Vec<Lexed>`.
pub fn lex<'a>(query: &'a str) -> Result<Vec<Lexed<'a>>, LexErr> {
  let lexed = pre_lex(query)?;
  let tokenized = tokenize(lexed)?;

  println!("{:#?}", tokenized);

  Ok(tokenized)
}