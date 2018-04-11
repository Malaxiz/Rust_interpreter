extern crate lang;

use std::io;
use std::io::{Write};

use std::env;
use std::fs::File;
use std::io::prelude::*;

use lang::interpreter::Interpreter;
use lang::vm::VM;

fn main() {

  let mut interpreter = Interpreter::new();
  let mut vm = VM::new();

  let mut f = File::open("test.lang")
    .expect("file not found");

  let mut contents = String::new();
  f.read_to_string(&mut contents)
    .expect("something went wrong reading the file");

  match lang::exec(&contents, &mut vm) {
    Ok(res) => print!("{}\n", res),
    Err(_) => {}
  };

  loop {
    print!("> ");
    io::stdout().flush().unwrap();

    let mut query = String::new();
    io::stdin().read_line(&mut query)
      .expect("Failed to read line");

    query.pop().unwrap();
    match lang::exec(&query, &mut vm) {
      Ok(res) => print!("{}\n", res),
      Err(_) => {}
    };
  }


}