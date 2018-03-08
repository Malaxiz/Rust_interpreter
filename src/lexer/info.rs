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
pub enum LexErr {
  MismatchedQuotes(i32),
  UnknownToken(String, i32),
  UnknownEscapeSequence(char, i32)
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Token {
  // operators
  Equals,
  Plus,
  Minus,
  Asterix,
  DoubleAsterix,
  Slash,
  Bang,

  SemiColon,
  Colon,
  Dot,
  DotDot,
  BraceClose,
  BraceOpen,
  Arrow,

  ParOpen,
  ParClose,
  
  EqualsEquals,
  BangEquals,
  LtOrEquals,
  GtOrEquals,
  Gt,
  Lt,

  LineBreak,
  Tab,

  // keywords
  If,
  Else,
  While,
  True,
  False,
  Nil,

  // macros
  Print,

  // pretend
  EOF,
  Literal,
  Identifier,
}

use self::Token::*;

pub fn get_tokens<'a>() -> HashMap<&'a str, Token> {
  map!{
    "=" => Equals,
    "+" => Plus,
    "-" => Minus,
    "*" => Asterix,
    "**" => DoubleAsterix,
    "/" => Slash,
    "!" => Bang,

    ";" => SemiColon,
    ":" => Colon,
    "." => Dot,
    ".." => DotDot,
    "{" => BraceOpen,
    "}" => BraceClose,
    "=>" => Arrow,

    "(" => ParOpen,
    ")" => ParClose,
    
    "==" => EqualsEquals,
    "!=" => BangEquals,
    ">=" => GtOrEquals,
    "<=" => LtOrEquals,
    ">" => Gt,
    "<" => Lt,

    "\n" => LineBreak,
    "\t" => Tab,

    "if" => If,
    "while" => While,
    "else" => Else,
    "true" => True,
    "false" => False,
    "nil" => Nil,

    "print!" => Print,

    "!@#$!@#$!@#$" => EOF
  }
}