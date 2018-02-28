use std::collections::HashMap;

macro_rules! map(
  { $($key:expr => $value:expr),+ } => {
    {
      let mut m = ::std::collections::HashMap::new();
        $(
          m.insert($key, $value);
        )+
      m
    }
  };
);

#[derive(Debug)]
pub enum LexErr<'a> {
  MismatchedQuotes(i32),
  UnknownToken(i32, &'a str),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Token {
  // operators
  Equals,
  Plus,
  Minus,
  Asterix,
  Slash,
  SemiColon,
  Colon,
  Dot,
  DotDot,

  ParOpen,
  ParClose,

  LtOrEq,
  GtOrEq,

  LineBreak,

  // keywords
  If,
}

use self::Token::*;

pub fn get_tokens<'a>() -> HashMap<&'a str, Token> {
  map!{
    "=" => Equals,
    "+" => Plus,
    "-" => Minus,
    "*" => Asterix,
    "/" => Slash,
    ";" => SemiColon,
    ":" => Colon,
    "." => Dot,
    ".." => DotDot,

    "(" => ParOpen,
    ")" => ParClose,
    
    ">=" => GtOrEq,
    "<=" => LtOrEq,

    "\n" => LineBreak,

    "if" => If
  }
}