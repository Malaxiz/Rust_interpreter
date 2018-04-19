use std::io;
use std::io::{Write};

use vm::exec::{VMExecError, Value, Literal, Function};
use vm::cast::{NativeScope, NativePars, NativeReturn};

fn literal_to_string(literal: &Literal, quotes: bool) -> String {
  let quotes = if quotes {"\""} else {""};
  match literal {
    &Literal::Num(val) => format!("{}", val),
    &Literal::Nil => format!("nil"),
    &Literal::Bool(b) => format!("{}", if b {"true"} else {"false"}),
    &Literal::String(ref val) => format!("{}{}{}", quotes, val, quotes),
    &Literal::Function(ref func_type) => match func_type {
      &Function::InCode(pos, ref arguments) => {
        let mut args = String::new();
        let mut first = true;
        for i in arguments {
          if !first {
            args += ", ";
          }
          args += i;
          first = false;
        }
        format!("<function ({}) at {}>", args, pos)
      },
      &Function::Native(ref func) => {
        format!("<native function at {:?}>", func)
      }
    },
    _ => format!("unknown literal (0)")
  }
}

fn non_literal_to_string(scope: NativeScope, val: *const Value, quotes: bool) -> Result<String, VMExecError> {
  unsafe {
      Ok(match &*val {
      &Value::Instance(scope) => format!("<instance at {:?}>", scope),
      _ => format!("unknown value")
    })
  }
}

pub fn value_to_string(scope: NativeScope, val: *const Value, quotes: bool) -> Result<String, VMExecError> {
  unsafe {
    Ok(match *val {
      Value::Literal(ref val) => literal_to_string(val, quotes),
      Value::Variable(ref identifier, pos) => match (*scope).get_var(identifier) {
        Some(val) => match *val {
          Value::Literal(ref val) => literal_to_string(val, quotes),
          _ => non_literal_to_string(scope, val, quotes)?
        },
        None => return Err(VMExecError::VariableNotDefined(identifier.to_string(), match pos {
          Some(pos) => pos,
          None => 0
        }))
      },
      _ => non_literal_to_string(scope, val, quotes)?
    })
  }
}

pub fn add_func(_scope: NativeScope, args: NativePars) -> NativeReturn {
  if args.len() < 2 {
    return Ok(None);
  }

  let first = args[0];
  let second = args[1];

  unsafe {
    match (&*first, &*second) {
      (&Value::Literal(ref first), &Value::Literal(ref second)) => match (first, second) {
        (&Literal::Num(first), &Literal::Num(second)) => Ok(Some(Value::Literal(Literal::Num(first + second)))),
        (_, _) => Ok(None)
      }
      (_, _) => Ok(None)
    }
  }
}

fn format(scope: NativeScope, args: NativePars) -> Result<String, VMExecError> {
  let mut res = String::new();
  for i in args {
    res += &format!("{}", value_to_string(scope, i, false)?);
  }
  Ok(res)
}

pub fn format_func(scope: NativeScope, args: NativePars) -> NativeReturn {
  let formatted = format(scope, args)?;
  Ok(Some(Value::Literal(Literal::String(formatted))))
}

pub fn print_func(scope: NativeScope, args: NativePars) -> NativeReturn {
  let formatted = format(scope, args)?;
  println!("{}", formatted);
  Ok(Some(Value::Literal(Literal::String(formatted))))
}

pub fn input_func(_scope: NativeScope, _args: NativePars) -> NativeReturn {
  io::stdout().flush().unwrap();
  let mut query = String::new();
  io::stdin().read_line(&mut query)
    .expect("Failed to read line");
  query.pop().unwrap();

  Ok(Some(Value::Literal(Literal::String(query))))
}