extern crate lang;

use std::io;
use std::io::{Write};

use std::env;
use std::fs::File;
use std::io::prelude::*;

use lang::vm;
use vm::{VM, BuildOptions};

fn shell(mut vm: &mut VM, options: BuildOptions) {
  loop {
    print!("> ");
    io::stdout().flush().unwrap();

    let mut query = String::new();
    io::stdin().read_line(&mut query)
      .expect("Failed to read line");

    query.pop().unwrap();

    let instructions = match lang::build(&query, &mut vm, options) {
      Ok(program) => program,
      Err(_) => continue
    };

    let program = vm::get_program(instructions);

    match lang::exec(program, &mut vm, true) {
      Ok(res) => println!("{}", res),
      Err(_) => continue
    }
  }
}

fn main() {
  let mut vm = VM::new();

  let args: Vec<_> = env::args().collect();
  if args.len() > 1 {
    if args.len() < 3 {
      panic!("need input file");
    }
    let cmd = &args[1];
    let name = &args[2];

    match cmd.as_ref() {
      "build" => {
        let mut output = "prog.lby";
        let mut release = false;
        let mut optimized = false;

        let mut i = 3;
        loop {
          if i >= args.len() {
            break;
          }
          match args[i].as_ref() {
            "-o" => {
              if i == args.len()-1 {
                panic!("usage: -o outputfile");
              }
              i += 1;
              output = &args[i];
            },
            "--release" => release = true,
            "--optimized" => optimized = true,
            _ => {
              panic!(format!("unknown option: {}", args[i]))
            }
          }
          i += 1;
        }

        let mut options = BuildOptions::DEBUG | BuildOptions::CODE;
        if release {
          options = BuildOptions::DEBUG;
        }
        if optimized {
          options = BuildOptions::NONE;
        }
        
        let mut f = File::open(name)
          .expect("file not found");

        let mut contents = String::new();
        f.read_to_string(&mut contents)
          .expect("something went wrong reading the file");

        match lang::build(&contents, &mut vm, options) {
          Ok(instructions) => {
            let mut file = File::create(output).unwrap();
            file.write_all(&instructions).unwrap();
            println!("build successful, program written to: {:?}", output);
          },
          Err(_) => {
            println!("build error");
            std::process::exit(-1);
          }
        };
      },
      "run" => {
        let mut shell_after = false;

        let mut i = 3;
        loop {
          if i >= args.len() {
            break;
          }
          match args[i].as_ref() {
            "--shell" => shell_after = true,
            _ => {
              panic!(format!("unknown option: {}", args[i]))
            }
          }
          i += 1;
        }
        
        let program = {
          let mut bytes = vec![];
          let mut f = File::open(name).unwrap();
          for byte in f.bytes() {
            bytes.push(byte.unwrap());
          }
          vm::get_program(bytes)
        };

        match lang::exec(program, &mut vm, false) {
          Ok(res) => println!("{}", res),
          Err(_) => {
            std::process::exit(-1);
          }
        }

        let is_debug = vm.vm_exec.is_debug;
        if shell_after {
          shell(&mut vm, if is_debug {
            BuildOptions::DEBUG | BuildOptions::CODE
          } else {
            BuildOptions::NONE
          });
        }

        std::process::exit(0);
      },
      _ => {}
    }
  } else {
    println!("Welcome to the shell!");
    shell(&mut vm, BuildOptions::DEBUG | BuildOptions::CODE);
  }
}