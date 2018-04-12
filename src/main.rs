extern crate lang;

use std::io;
use std::io::{Write};

use std::env;
use std::fs::File;
use std::io::prelude::*;

use lang::vm;
use vm::VM;

fn main() {

  let mut vm = VM::new();

  let args: Vec<_> = env::args().collect();
  if args.len() > 1 {
    if args.len() < 3 {
      panic!("wrong usage");
    }
    let cmd = &args[1];
    let name = &args[2];

    match cmd.as_ref() {
      "build" => {
        let mut output = "prog.lby";
        if args.len() > 3 {
          output = &args[3];
        }
        
        let mut f = File::open(name)
          .expect("file not found");

        let mut contents = String::new();
        f.read_to_string(&mut contents)
          .expect("something went wrong reading the file");

        match lang::build(&contents, &mut vm) {
          Ok(instructions) => {
            let mut file = File::create(output).unwrap();
            file.write_all(&instructions).unwrap();
            println!("build successful, program written to: {:?}", output);
          },
          Err(_) => println!("build error")
        };
      },
      "run" => {
        let program = {
          let mut bytes = vec![];
          let mut f = File::open(name).unwrap();
          for byte in f.bytes() {
            bytes.push(byte.unwrap());
          }
          vm::get_program(bytes)
        };

        match lang::exec(program, &mut vm) {
          Ok(res) => println!("{}", res),
          Err(_) => {}
        }
      },
      _ => {}
    }
  } else {
    loop {
      print!("> ");
      io::stdout().flush().unwrap();

      let mut query = String::new();
      io::stdin().read_line(&mut query)
        .expect("Failed to read line");

      query.pop().unwrap();

      let instructions = match lang::build(&query, &mut vm) {
        Ok(program) => program,
        Err(_) => continue
      };

      let program = vm::get_program(instructions);
      println!("{:#?}", program);

      match lang::exec(program, &mut vm) {
        Ok(res) => println!("{}", res),
        Err(_) => continue
      }
    }
  }
}