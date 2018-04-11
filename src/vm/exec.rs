use vm::*;
use vm::OPCode::*;
use enum_primitive::FromPrimitive;

#[derive(Debug)]
pub enum VMExecError {
  // error, pos
  UnsupportedOperation(Literal, Literal, OPCode, i32),
  InvalidOPCode(String),
  UnsupportedOPCode(String),

  // position in bytecode
  InvalidOperationContent(usize),

  Temp
}

#[derive(Clone, Debug)]
pub enum Literal {
  Num(f64),
  Bool(bool),
  String(String)
}

#[derive(Clone, Debug)]
pub enum Value {
  // Variable(),
  Literal(Literal),
  NIL,

  None,
}

struct Root {
  pool: Vec<Box<Value>>,
  literal_strs: Vec<Box<String>>
}

impl Root {
  pub fn new() -> Self {
    Self {
      pool: Vec::new(),
      literal_strs: Vec::new()
    }
  }

  pub fn gc() {
    // todo
  }
}

pub struct VMExec {
  op_i: usize,
  program: Vec<Operation>,

  root: Root,
  stack: [*const Value; 512],
  stacki: usize,

  query: String
}

impl VMExec {
  pub fn new() -> Self {
    Self {
      op_i: 0,
      program: Vec::new(),

      root: Root::new(),
      stack: [&Value::None; 512],
      stacki: 0,

      query: String::from("")
    }
  }

  fn reset(&mut self) {
    self.program = Vec::new();
    self.op_i = 0;
    self.stack = [&Value::None; 512];
    self.stacki = 0;
    self.query = String::from("");
  }

  fn consume(&mut self) -> u8 {
    self.op_i += 1;
    self.program[self.op_i].val
  }

  fn stack_push(&mut self, val: *const Value) {
    self.stack[self.stacki] = val;
    self.stacki += 1;
  }

  fn stack_pop(&mut self) -> *const Value {
    if self.stacki <= 0 {
      return NIL;
    }
    self.stacki -= 1;
    self.stack[self.stacki]
  }

  fn literal_operation(&mut self, val1: *const Value, val2: *const Value, operation: &OPCode, pos: Option<i32>) -> Result<*const Value, VMExecError> {
    let get_pos = || {
      match pos {
        Some(pos) => pos,
        _ => -1
      }
    };

    unsafe {
      match (&*val1, &*val2) {
        (&Value::Literal(ref val1), &Value::Literal(ref val2)) => {
          let res: Value = match(val1, val2, operation) {

            // NUMBER OPERATIONS
            (&Literal::Num(first), &Literal::Num(second), &ADD) => {
              Value::Literal(Literal::Num(first + second))
            },
            (&Literal::Num(first), &Literal::Num(second), &SUB) => {
              Value::Literal(Literal::Num(first - second))
            },
            (&Literal::Num(first), &Literal::Num(second), &MULTIPLY) => {
              Value::Literal(Literal::Num(first * second))
            },

            // STRING OPERATIONS
            (&Literal::String(ref first), &Literal::Num(second), &MULTIPLY) |
            (&Literal::Num(second), &Literal::String(ref first), &MULTIPLY) => {
              Value::Literal(Literal::String(format!{"{}", first}.repeat(second as usize)))
            },
            (&Literal::String(ref first), &Literal::Num(second), &ADD) => {
              Value::Literal(Literal::String(format!("{}{}", first, second)))
            },
            (&Literal::Num(first), &Literal::String(ref second), &ADD) => {
              Value::Literal(Literal::String(format!("{}{}", first, second)))
            },

            _ => return Err(VMExecError::UnsupportedOperation(val1.clone(), val2.clone(), operation.clone(), get_pos()))
          };

          let res = Box::new(res);
          let res_point: *const Value = &*res;
          self.root.pool.push(res);
          Ok(res_point)
        },
        _ => return Err(VMExecError::Temp)
      }
    }
  }

  fn print_stack(self) {
    print!("stack: {}\n---------\n", self.stacki);
    for (i, v) in self.stack.iter().enumerate() {
      unsafe {
        if *v != &Value::None {
          print!("    {}: {:?}\n", i, **v);
        }
      }
    }
    print!("---------\n");
  }

  pub fn exec(&mut self, program: Program) -> Result<String, VMExecError> {
    self.reset();
    // println!("root: {:?}", self.root.pool);
    self.program = program;

    let mut meta_end = false;
    let mut is_debug = false;

    let mut self_point: *mut Self = self;

    loop {
      let op: &Operation = unsafe {
        &((*self_point).program[self.op_i])
      };
      let code: &Option<OPCode> = &op.code;
      let content = &op.content;

      if let &Some(ref code) = code {
        if !meta_end {
          match *code {
            META_END => meta_end = true,
            DEBUG => is_debug = true,
            DEBUG_CODE => {
              let mut query: Vec<u8> = Vec::new();
              loop {
                self.op_i += 1;
                let op: &Operation = unsafe {
                  &((*self_point).program[self.op_i])
                };
                let code: &Option<OPCode> = &op.code;
                if let &Some(ref code) = code {
                  if code == &DEBUG_CODE_END {
                    break;
                  }
                }
                
                query.push(op.val);
              }

              let query = match str::from_utf8(&query) {
                Ok(v) => v,
                Err(e) => panic!("Invalid UTF-8 sequence: {}", e),
              };

              self.query = query.to_string();
            },
            _ => {}
          }
          
          self.op_i += 1;
          continue;
        }
        
        match *code {
          END => {
            unsafe {
              return Ok(format!("{:?}", *self.stack_pop()));
            }
          },
          PUSH_NUM => {
            let val = Box::new(Value::Literal(Literal::Num(match content {
              &OperationLiteral::Num(num) => num,
              _ => return Err(VMExecError::InvalidOperationContent(self.op_i))
            })));
            self.op_i += 8; // offset of f64
            let val_point = &*val as *const Value;
            self.root.pool.push(val);
            self.stack_push(val_point);
          },
          PUSH_BOOL => {
            let b = self.consume();
            let val = Box::new(Value::Literal(Literal::Bool(if b >= 1 {true} else {false})));
            let val_point = &*val as *const Value;
            self.root.pool.push(val);
            self.stack_push(val_point);
          },
          PUSH_STRING => {
            let val = Box::new(Value::Literal(Literal::String(match content {
              &OperationLiteral::String(ref s, len) => {
                self.op_i += len; // offset of string
                s.to_owned()
              },
              _ => return Err(VMExecError::InvalidOperationContent(self.op_i))
            })));
            let val_point = &*val as *const Value;
            self.root.pool.push(val);
            self.stack_push(val_point);
          },
          ADD | SUB | MULTIPLY => {
            let mut pos = None;
            if is_debug {
              pos = Some(self.consume() as i32);
            }

            let second = self.stack_pop();
            let first = self.stack_pop();
            let res = self.literal_operation(first, second, code, pos)?;
            self.stack_push(res);
          },
          POP => {
            self.stack_pop();
          }
          _ => return Err(VMExecError::UnsupportedOPCode(format!("{:?}", op.val)))
        };

        self.op_i += 1;
      } else {
        return Err(VMExecError::InvalidOPCode(format!("{:?}", op.val)))
      }
    }
  }
}