//! The parser module rearranges the `Vec<Lexed>` result from the lexer module into an easily executable set of instructions.

pub mod info;
pub mod grammar;

pub use self::grammar::Expression;
pub use self::grammar::Primary;
pub use self::info::ParserErr;

use std::collections::HashMap;

use lexer::Token;
use lexer::Token::*;
use lexer::Lexed;
use lexer::get_tokens;

// fn get_string_from_token<'a>(token: &Token, tokens: &HashMap<&'a str, Token>) -> Option<&'a str> {
//   for (k, v) in tokens {
//     if token == v {
//       return Some(k);
//     }
//   }
//   None
// }

/// Checks the instructions and checks if they're allowed
pub fn check(lexed: &Vec<Lexed>) -> Result<(), ParserErr> {

  // let atokens: HashMap<&str, Token> = get_tokens();

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
      &Lexed::Literal(ref _literal, pos) => {
        if !allowed_literal {
          return Err(ParserErr::UnexpectedLiteral(pos));
        } else {

        }
      },
      &Lexed::Identifier(ref _name, pos) => {
        if !allowed_identifier {
          return Err(ParserErr::UnexpectedIdentifier(pos));
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

          match token {
            Token::LineBreak => return Err(ParserErr::UnexpectedEndOfLine(pos)),
            _ => return Err(ParserErr::UnexpectedToken(pos, token, allowed_operators, allowed_literal, allowed_identifier))
          };
        }
      }
    }

    allowed_operators.clear();
    allowed_literal = false;
    allowed_identifier = false;

    match t {
      &Lexed::Literal(ref _literal, _pos) => {
        allowed_operators = vec![
          Plus, Minus, Asterix, Slash, Dot, ParClose, SemiColon, LineBreak
        ];
      },
      &Lexed::Identifier(ref _name, _pos) => {
        allowed_operators = vec![
          Equals, Plus, Minus, Asterix, Slash, Dot, ParClose, SemiColon, LineBreak, ParOpen
        ];
      },
      &Lexed::Operator(token, _pos) => {
        match token {
          Equals => {
            allowed_identifier = true;
            allowed_literal = true;
            allowed_operators = vec![
              ParOpen, Minus
            ];
          }
          Plus | Minus | Asterix | Slash => {
            allowed_literal = true;
            allowed_identifier = true;
            allowed_operators = vec![
              ParOpen, Minus
            ];
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
              ParClose, ParOpen, Plus, Minus, Asterix, Slash, Dot, SemiColon, LineBreak
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

  Ok(())
}

fn ast<'a>(checked: &Vec<Lexed>) -> Result<Expression, ParserErr> {
  let mut g = grammar::Grammar::new(checked);
  g.expression()
}

pub fn parse(lexed: &Vec<Lexed>) -> Result<Vec<Expression>, ParserErr> {
  let checked = check(lexed)?;
  let ast = ast(lexed)?;

  Ok(vec![ast])
}