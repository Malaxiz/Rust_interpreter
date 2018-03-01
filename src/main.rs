extern crate lang;

use std::io;

fn main() {

  loop {
    let mut query = String::new();
    io::stdin().read_line(&mut query)
      .expect("Failed to read line");

    let result = lang::exec(&query);
  }
}