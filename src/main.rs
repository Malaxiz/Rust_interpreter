extern crate lang;

use std::io;
use std::io::{Write};

use lang::interpreter::Interpreter;

fn main() {

  let mut interpreter = Interpreter::new();

  loop {
    print!("> ");
    io::stdout().flush();

    let mut query = String::new();
    io::stdin().read_line(&mut query)
      .expect("Failed to read line");

    let result = lang::exec(&query, &mut interpreter);
  }
}