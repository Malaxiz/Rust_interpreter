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
  Comma,
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
  Let,
  Ref,
  DeRef,
  Struct,
  If,
  Else,
  While,
  Func,
  True,
  False,
  Nil,

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
    "," => Comma,
    "{" => BraceOpen,
    "}" => BraceClose,
    // "=>" => Arrow,

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

    "let" => Let,
    "&" => Ref,
    // "*" => DeRef,
    "struct" => Struct,
    "if" => If,
    "while" => While,
    "else" => Else,
    "fn" => Func,
    "true" => True,
    "false" => False,
    "nil" => Nil,

    "!@#$!@#$!@#$" => EOF
  }
}