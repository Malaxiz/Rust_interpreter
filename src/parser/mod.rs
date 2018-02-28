//! The parser module rearranges the `Vec<Lexed>` result from the lexer module into an easily executable set of instructions.

mod info;

pub use self::info::ParserErr;

use std::collections::HashMap;

use lexer::Token;
use lexer::Token::*;
use lexer::Lexed;
use lexer::get_tokens;

fn get_string_from_token<'a>(token: &Token, tokens: &HashMap<&'a str, Token>) -> Option<&'a str> {
  for (k, v) in tokens {
    if token == v {
      return Some(k);
    }
  }
  None
}

/// Scans the instructions and checks if they're allowed
fn scan<'a>(lexed: &Vec<Lexed>) -> Result<bool, ParserErr<'a>> {

  let atokens: HashMap<&str, Token> = get_tokens();

  let mut allowed_literal = true;
  let mut allowed_identifier = true;
  let mut allowed_operators: Vec<Token> = vec![
    ParOpen,
    LineBreak,
    SemiColon,
  ];

  let mut i = 0;
  let len = lexed.len();
  while i < len {
    let t = &lexed[i];

    match t {
      &Lexed::Literal(ref literal, pos) => {
        if !allowed_literal {
          return Err(ParserErr::UnexpectedLiteral(pos, "[literal]"));
        } else {

        }
      },
      &Lexed::Identifier(name, pos) => {
        if !allowed_identifier {
          return Err(ParserErr::UnexpectedIdentifier(pos, "[identifier]"));
        } else {

        }
      },
      &Lexed::Operator(token, pos) => {
        if !allowed_operators.contains(&token) {
          // let s = if let Some(val) = get_string_from_token(&token, &atokens) {
          //   val
          // } else {
          //   "unknown token"
          // };

          return Err(ParserErr::UnexpectedToken(pos, token));
        }
      }
    }

    allowed_operators.clear();
    allowed_literal = false;
    allowed_identifier = false;

    match t {
      &Lexed::Literal(ref literal, pos) => {
        allowed_operators = vec![
          Plus, Minus, Asterix, Slash, Dot, ParClose, SemiColon, LineBreak
        ];
      },
      &Lexed::Identifier(name, pos) => {
        allowed_operators = vec![
          Plus, Minus, Asterix, Slash, Dot, ParClose, SemiColon, LineBreak, ParOpen
        ];
      },
      &Lexed::Operator(token, pos) => {
        match token {
          Plus | Minus | Asterix | Slash => {
            allowed_literal = true;
            allowed_identifier = true;
          },
          Dot => {
            allowed_identifier = true;
          },
          ParOpen => {
            allowed_identifier = true;
            allowed_literal = true;
            allowed_operators = vec![
              ParClose
            ];
          },
          ParClose => {
            allowed_operators = vec![
              ParOpen, Plus, Minus, Asterix, Slash, Dot, SemiColon, LineBreak
            ];
          },
          SemiColon => {
            allowed_identifier = true;
            allowed_literal = true;
            allowed_operators = vec![
              SemiColon, LineBreak, ParOpen
            ];
          }
          _ => {

          }
        }
      }
    }

    i += 1;
  }

  Ok(true)
}

pub fn parse<'a>(lexed: &Vec<Lexed<'a>>) -> Result<Vec<Lexed<'a>>, ParserErr<'a>> {
  let scanned = scan(lexed).unwrap();

  Ok(vec![])
}