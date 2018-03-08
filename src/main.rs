extern crate lang;

use std::io;
use std::io::{Write};

use std::env;
use std::fs::File;
use std::io::prelude::*;

use lang::interpreter::Interpreter;

fn main() {

  let mut interpreter = Interpreter::new(None);

  let mut f = File::open("test.lang")
    .expect("file not found");

  let mut contents = String::new();
  f.read_to_string(&mut contents)
    .expect("something went wrong reading the file");

  lang::exec(&contents, &mut interpreter);

  loop {
    print!("> ");
    io::stdout().flush().unwrap();

    let mut query = String::new();
    io::stdin().read_line(&mut query)
      .expect("Failed to read line");

    query.pop().unwrap();
    match lang::exec(&query, &mut interpreter) {
      Ok(res) => print!("{}\n", res),
      Err(_) => {}
    };
  }


}