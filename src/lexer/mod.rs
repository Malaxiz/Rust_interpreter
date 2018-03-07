//! The lexer module function `lex` takes a `query: &str` and lexes it into a `Vec<Lexed>` which can then be parsed.

mod info;

pub use self::info::Token;
pub use self::info::LexErr;
pub use self::info::get_tokens;

use std::collections::HashMap;

#[derive(Debug)]
enum PreLexed<'a> {
  String(String, i32),
  Rest(&'a str, i32),
  Comment(&'a str)
}

#[derive(Debug, Clone)]
pub enum Literal {
  String(String),
  Num(f64),
  Bool(bool),
  Nil,
  // Function(Vec<String>, ),

  Variable(String)
}

#[derive(Debug)]
pub enum Lexed {
  Literal(Literal, i32),
  Operator(Token, i32),
  Identifier(String, i32),
}

fn remove_comments(query: &str) -> Result<Vec<PreLexed>, LexErr> {
  let mut pre_lexed = Vec::new();

  let mut is_comment = false;
  let mut prev_char;
  let mut curr_char = ' ';
  let mut start = 0;

  for (i, c) in query.chars().enumerate() {
    prev_char = curr_char;
    curr_char = c;

    match c {
      '/' => {
        if is_comment {
          continue;
        }
        if prev_char == '/' {
          is_comment = true;
          pre_lexed.push(PreLexed::Rest(&query[start..i-1], start as i32));
          start = i;
        }
      },
      '\n' => {
        if is_comment {
          is_comment = false;
          pre_lexed.push(PreLexed::Comment(&query[start-1..i]));
          start = i;
        }
      },
      _ => { }
    }
  }

  pre_lexed.push(if is_comment {
    PreLexed::Comment(&query[start-1..query.len()])
  } else {
    PreLexed::Rest(&query[start..query.len()], query.len() as i32)
  });

  Ok(pre_lexed)
}

fn resolve_escapes<'a>(query: &str, pos: i32) -> Result<String, LexErr> {
  let mut s = String::from("");

  fn get_seq<'a>(c: char, pos: i32) -> Result<char, LexErr<'a>> {
    Ok(match c {
      'n' => '\n',
      _ => return Err(LexErr::UnknownEscapeSequence(c, pos))
    })
  }

  let mut in_escape = false;

  for (k, v) in query.chars().enumerate() {
    match v {
      '\\' => {
        if in_escape {
          s.push(v);
        }

        in_escape = !in_escape;
      }
      _ => {
        s.push(if in_escape {
          in_escape = false;
          get_seq(v, pos + k as i32)?
        } else {
          v
        });
      }
    }
  }

  Ok(s)
}

fn pre_lex(query: &str) -> Result<Vec<PreLexed>, LexErr> {
  let removed_comments = remove_comments(query)?;

  let mut pre_lexed: Vec<PreLexed> = Vec::new();

  let mut comment_offset = 0;
  let mut rest_offset = 0;

  for (n, t) in removed_comments.iter().enumerate() {
    match t {
      &PreLexed::Comment(ref c) => {
        comment_offset += c.len();
      },
      &PreLexed::Rest(ref query, pos) => {
        let mut quote_count = 0;
        let mut in_quote = false;
        let mut start = 0;
        let mut escaped = false;

        for (i, c) in query.chars().enumerate() {
          match c {
            '\\' => {
              escaped = !escaped;
              continue;
            },
            '"' => {
              if escaped {
                escaped = false;
                continue;
              }

              quote_count += 1;
              in_quote = !in_quote;

              let s = &query[start..i];
              let apos = start as i32 + comment_offset as i32 + rest_offset as i32;
              pre_lexed.push(if in_quote {
                PreLexed::Rest(s, apos)
              } else {
                PreLexed::String(resolve_escapes(s, apos)?, apos)
              });

              start = i+1;
            },
            _ => {
              escaped = false;
            }
          }
        }

        if quote_count % 2 != 0 {
          return Err(LexErr::MismatchedQuotes((start - 1) as i32 + comment_offset as i32 + rest_offset as i32));
        }

        pre_lexed.push(PreLexed::Rest(&query[start..query.len()], start as i32 + comment_offset as i32 + rest_offset as i32));

        rest_offset += query.len() as i32;
      },
      _ => {  }
    }
  }

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

  // if val.len() == 1 && val.chars().next() == Some(' ') {
  //   return (val, 0, 0);
  // }

  let mut start: i32 = 0;
  let mut end: i32 = val.len() as i32 - 1;

  for (k, v) in val.chars().enumerate() {
    match v {
      ' ' | '\t' => {}
      _ => {
        start = k as i32;
        break;
      }
    }
  };

  for (k, v) in val.chars().rev().enumerate() {
    match v {
      ' ' | '\t' => {}
      _ => {
        end -= k as i32 - 1;
        break;
      }
    }
  };

  (&val[start as usize..end as usize], start, end)
}

fn tokenize(pre_lexed: Vec<PreLexed>) -> Result<Vec<Lexed>, LexErr> {

  let prev_tokens: HashMap<&str, Token> = info::get_tokens();

  let tokens: Vec<(&&str, &Token)> = prev_tokens.iter().collect();
  // tokens.sort_by(|a, b| b.0.len().cmp(&a.0.len()));

  let mut lexed: Vec<Lexed> = Vec::new();
  for i in pre_lexed {
    match i {
      PreLexed::Comment(_) => {

      },
      PreLexed::Rest(val, pos) => {
        let val = trim(val).0;

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
            match op {
              Token::True => lexed.push(Lexed::Literal(Literal::Bool(true), apos)),
              Token::False => lexed.push(Lexed::Literal(Literal::Bool(false), apos)),
              Token::Nil => lexed.push(Lexed::Literal(Literal::Nil, apos)),
              _ => lexed.push(Lexed::Operator(op, apos))
            }
          } else if is_identifier(trimmed.0) {
            lexed.push(Lexed::Identifier(String::from(trimmed.0), apos));
          } else if is_number(trimmed.0) {
            lexed.push(Lexed::Literal(Literal::Num(trimmed.0.parse::<f64>().unwrap()), apos));
          } else if trimmed.0.len() <= 1 {
            return Err(LexErr::UnknownToken(trimmed.0, apos));
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

  // remove linebreaks
  lexed.retain(|&ref i| match i {
    &Lexed::Operator(op, _) => {
      op != Token::LineBreak &&
      op != Token::Tab
    }
    _ => true
  });

  fn get_string_from_token<'a>(token: &Token, tokens: &HashMap<&'a str, Token>) -> Option<&'a str> {
    for (k, v) in tokens {
      if token == v {
        return Some(k);
      }
    }
    None
  }

  let last_pos: i32 = if lexed.len() >= 1 {
    let last_elem = &lexed[lexed.len() - 1];
    match last_elem {
      &Lexed::Identifier(ref val, ref pos) => val.len() as i32 + *pos,
      &Lexed::Literal(ref literal, ref pos) => (match literal {
        &Literal::Bool(b) => (if b { "true".len() } else { "false".len() }) as i32,
        &Literal::Nil => "nil".len() as i32,
        &Literal::Num(i) => i.to_string().len() as i32,
        &Literal::String(ref s) => s.len() as i32,
        &Literal::Variable(_) => 0
      }) + *pos,
      &Lexed::Operator(ref operator, ref pos) => (match get_string_from_token(operator, &prev_tokens) {
        Some(val) => val.len() as i32,
        None => 0
      }) + *pos
    }
  } else {
    0
  };
  lexed.push(Lexed::Operator(Token::EOF, last_pos));

  Ok(lexed)
}

/// Lexes a `query: &str` into a vector of tokens: `Vec<Lexed>`.
pub fn lex(query: &str) -> Result<Vec<Lexed>, LexErr> {
  let lexed = pre_lex(query)?;
  let tokenized = tokenize(lexed)?;
  Ok(tokenized)
}