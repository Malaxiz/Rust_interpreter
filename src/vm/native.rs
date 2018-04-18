use vm::exec::{Value, Literal};
use vm::cast::{NativePars, NativeReturn};

pub fn add(args: NativePars) -> NativeReturn {
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