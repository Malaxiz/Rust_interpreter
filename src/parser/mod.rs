//! The parser module rearranges the `Vec<Lexed>` result from the lexer module into an easily executable set of instructions.

pub mod info;
pub mod grammar;

pub use self::grammar::Program;
pub use self::grammar::Declaration;
pub use self::grammar::Statement;
pub use self::grammar::Expression;
pub use self::grammar::Primary;
pub use self::info::ParserErr;

use lexer::Token;
use lexer::Token::*;
use lexer::Lexed;

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
    Bang,
    Minus,
    EOF,
    Print,
    If
  ];

  let mut i = 0;
  let len = lexed.len();
  while i < len {
    let t = &lexed[i];

    match t {
      &Lexed::Literal(ref _literal, pos) => {
        if !allowed_literal {
          println!("not allowed: {:?}", _literal);
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
          Plus, Minus, Asterix, DoubleAsterix, Slash, Dot, ParClose, SemiColon,
          BraceOpen, BraceClose,
          EqualsEquals, BangEquals, LtOrEquals, GtOrEquals, Gt, Lt
        ];
      },
      &Lexed::Identifier(ref _name, _pos) => {
        allowed_operators = vec![
          Equals, Plus, Minus, Asterix, DoubleAsterix, Slash, Dot, ParClose, SemiColon, ParOpen,
          BraceOpen, BraceClose,
          EqualsEquals, BangEquals, LtOrEquals, GtOrEquals, Gt, Lt
        ];
      },
      &Lexed::Operator(token, _pos) => {
        match token {
          Equals => {
            allowed_identifier = true;
            allowed_literal = true;
            allowed_operators = vec![
              ParOpen, Minus, Bang, If
            ];
          },
          Plus | Minus | Asterix | Slash | Bang | DoubleAsterix => {
            allowed_literal = true;
            allowed_identifier = true;
            allowed_operators = vec![
              ParOpen, Minus, Bang
            ];
          },
          EqualsEquals | BangEquals | LtOrEquals | GtOrEquals | Gt | Lt => {
            allowed_literal = true;
            allowed_identifier = true;
          }
          Dot => {
            allowed_identifier = true;
          },
          ParOpen => {
            allowed_identifier = true;
            allowed_literal = true;
            allowed_operators = vec![
              ParClose, ParOpen
            ];
          },
          ParClose => {
            allowed_operators = vec![
              ParClose, ParOpen, Plus, Minus, Asterix, Slash, Dot, SemiColon, BraceOpen
            ];
          },
          SemiColon => {
            allowed_identifier = true;
            allowed_literal = true;
            allowed_operators = vec![
              SemiColon, ParOpen, EOF, BraceClose, If,
              Print
            ];
          },
          Print => {
            allowed_identifier = true;
            allowed_literal = true;
            allowed_operators = vec![
              Minus, Bang, ParOpen
            ];
          },
          If => {
            allowed_identifier = true;
            allowed_literal = true;
            allowed_operators = vec![
              Minus, Bang, ParOpen
            ];
          },
          Else => {
            allowed_operators = vec![
              BraceOpen
            ];
          },
          BraceOpen => {
            allowed_identifier = true;
            allowed_literal = true;
            allowed_operators = vec![
              Minus, Bang, ParOpen, Print, If, BraceClose
            ];
          },
          BraceClose => {
            // allowed_identifier = true;
            // allowed_literal = true;
            allowed_operators = vec![
              Else, SemiColon, BraceClose
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

pub fn parse(lexed: &Vec<Lexed>) -> Result<Vec<Box<Declaration>>, ParserErr> {
  check(lexed)?;
  
  let mut g = grammar::Grammar::new(lexed);
  let program = g.program()?;

  Ok(program)
}