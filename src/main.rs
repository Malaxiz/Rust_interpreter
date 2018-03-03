extern crate lang;

use std::io;
use std::io::{Write};

fn main() {

  loop {
    print!("> ");
    io::stdout().flush();

    let mut query = String::new();
    io::stdin().read_line(&mut query)
      .expect("Failed to read line");

    let result = lang::exec(&query);
  }
}