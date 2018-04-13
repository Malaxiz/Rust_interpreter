use vm::*;
use vm::OPCode::*;
use enum_primitive::FromPrimitive;
use std::collections::HashMap;

#[derive(Debug)]
pub enum VMExecError {
  // error, pos
  UnsupportedOperation(Literal, Literal, OPCode, i32),
  InvalidOPCode(String),
  UnsupportedOPCode(String),
  VariableNotDefined(String, i32),

  // position in bytecode
  InvalidOperationContent(usize),
  InvalidIdentifier(String),
  InvalidCast(Literal, String, i32),

  Temp
}

#[derive(Clone, Debug)]
pub enum Literal {
  Num(f64),
  Bool(bool),
  String(String),
  Nil
}

#[derive(Clone, Debug)]
pub enum Value {
  Variable(String, Option<i32>),
  Literal(Literal),
  None,
}

// fn get_binary_pos(Vec<u8>)

struct Root {
  pool: Vec<Box<Value>>
}

impl Root {
  pub fn new() -> Self {
    Self {
      pool: Vec::new()
    }
  }

  pub fn gc() {
    // todo
  }
}

pub struct VMExec {
  op_i: i32,
  program: Vec<Operation>,

  variables: HashMap<String, *const Value>,
  root: Root,
  stack: [*const Value; 512],
  stacki: usize,

  pub query: String,

  pub is_debug: bool,
  pub contains_code: bool
}

impl VMExec {
  pub fn new() -> Self {
    Self {
      op_i: 0,
      program: Vec::new(),

      variables: HashMap::new(),
      root: Root::new(),
      stack: [&Value::None; 512],
      stacki: 0,

      query: String::from(""),

      is_debug: false,
      contains_code: false,
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
    self.program[self.op_i as usize].val
  }

  fn consume_op(&mut self) -> &Operation {
    self.op_i += 1;
    &self.program[self.op_i as usize]
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

  fn stack_peek(&self) -> *const Value {
    if self.stacki <= 0 {
      return NIL;
    }
    let val = self.stack[self.stacki - 1];
    unsafe {
      match *val {
        Value::None => NIL,
        _ => val
      }
    }
  }

  fn get_int(&mut self) -> Result<i32, VMExecError> {
    let self_point: *mut Self = self;
    let int = self.consume_op();
    unsafe {
      (*self_point).op_i += 4;
    }
    Ok(match int.content {
      OperationLiteral::Pos(pos) => pos,
      _ => return Err(VMExecError::Temp)
    })
  }

  fn get_debug_pos(&mut self) -> Result<Option<i32>, VMExecError> {
    if self.is_debug {
      Ok(Some(self.get_int()?))
    } else {
      Ok(None)
    }
  }

  fn literal_to_string(&self, literal: &Literal, quotes: bool) -> String {
    let quotes = if quotes {"\""} else {""};
    match literal {
      &Literal::Num(val) => format!("{}", val),
      &Literal::Nil => format!("nil"),
      &Literal::Bool(b) => format!("{}", if b {"true"} else {"false"}),
      &Literal::String(ref val) => format!("{}{}{}", quotes, val, quotes),
      _ => format!("err")
    }
  }

  fn value_to_string(&self, val: *const Value, quotes: bool) -> String {
    unsafe {
      match *val {
        Value::Literal(ref val) => self.literal_to_string(val, quotes),
        Value::Variable(ref identifier, _) => match self.variables.get(identifier) {
          Some(val) => match **val {
            Value::Literal(ref val) => self.literal_to_string(val, quotes),
            _ => format!("err")
          },
          None => format!("nil")
        },
        _ => format!("None")
      }
    }
  }

  fn literal_operation(&mut self, val1f: *const Value, val2f: *const Value, operation: &OPCode, pos: Option<i32>) -> Result<*const Value, VMExecError> {
    let get_pos = || {
      match pos {
        Some(pos) => pos,
        _ => -1
      }
    };

    let is_assignment = operation == &ASSIGN;

    let mut val1 = val1f;
    let mut val2 = val2f;

    unsafe {
      if !is_assignment {
        if let &Value::Variable(ref identifier, pos) = &*val1f {
          val1 = match self.variables.get(identifier) {
            Some(val) => *val,
            None => return Err(VMExecError::VariableNotDefined(identifier.to_string(), match pos {
              Some(pos) => pos,
              None => 0
            }))
          }
        }
      }
      if let &Value::Variable(ref identifier, pos) = &*val2f {
        val2 = match self.variables.get(identifier) {
          Some(val) => *val,
          None => return Err(VMExecError::VariableNotDefined(identifier.to_string(), match pos {
            Some(pos) => pos,
            None => 0
          }))
        }
      }
      // println!("{:?}, {:?}", *val1, *val2);
    }

    unsafe {
      match (&*val1, &*val2) {
        (&Value::Literal(ref lit1), &Value::Literal(ref lit2)) => {
          let res: Value = match(lit1, lit2, operation) {

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
            (&Literal::Num(first), &Literal::Num(second), &DIVIDE) => {
              Value::Literal(Literal::Num(first / second))
            },

            // BOOLEAN OPERATIONS
            (&Literal::Num(first), &Literal::Num(second), &LT) => {
              Value::Literal(Literal::Bool(first < second))
            },
            (&Literal::Num(first), &Literal::Num(second), &GT) => {
              Value::Literal(Literal::Bool(first > second))
            },
            (&Literal::Num(first), &Literal::Num(second), &LTOREQ) => {
              Value::Literal(Literal::Bool(first <= second))
            },
            (&Literal::Num(first), &Literal::Num(second), &GTOREQ) => {
              Value::Literal(Literal::Bool(first <= second))
            },

            // STRING OPERATIONS
            (&Literal::String(ref first), &Literal::Num(second), &MULTIPLY) |
            (&Literal::Num(second), &Literal::String(ref first), &MULTIPLY) => {
              Value::Literal(Literal::String(format!{"{}", first}.repeat(second as usize)))
            },
            (&Literal::String(ref first), &Literal::Num(second), &ADD) => {
              Value::Literal(Literal::String(format!("{}{}", first, second)))
            },
            (&Literal::String(ref first), &Literal::String(ref second), &ADD) => {
              Value::Literal(Literal::String(format!("{}{}", first, second)))
            },
            (&Literal::Num(first), &Literal::String(ref second), &ADD) => {
              Value::Literal(Literal::String(format!("{}{}", first, second)))
            },

            _ => return Err(VMExecError::UnsupportedOperation(lit1.clone(), lit2.clone(), operation.clone(), get_pos()))
          };

          let res = Box::new(res);
          let res_point: *const Value = &*res;
          self.root.pool.push(res);
          Ok(res_point)
        },
        (&Value::Variable(ref identifier, _), &Value::Literal(ref lit)) => {
          match (lit, operation) {
            (_, &ASSIGN) => {
              self.variables.insert(identifier.to_string(), val2);
              Ok(val2) // might be memory error
            },
            _ => {
              Ok(NIL)
            }
          }
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
    let mut self_point: *mut Self = self;

    loop {
      let op: &Operation = unsafe {
        &((*self_point).program[self.op_i as usize])
      };
      let code: &Option<OPCode> = &op.code;
      let content = &op.content;

      if let &Some(ref code) = code {
        if !meta_end {
          match *code {
            META_END => meta_end = true,
            DEBUG => self.is_debug = true,
            DEBUG_CODE => {
              self.contains_code = true;
              let mut query: Vec<u8> = Vec::new();
              loop {
                self.op_i += 1;
                let op: &Operation = unsafe {
                  &((*self_point).program[self.op_i as usize])
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
              return Ok((*self_point).value_to_string(self.stack_pop(), true));
            }
          },
          PUSH_NUM => {
            let val = Box::new(Value::Literal(Literal::Num(match content {
              &OperationLiteral::Num(num) => num,
              _ => return Err(VMExecError::InvalidOperationContent(self.op_i as usize))
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
                self.op_i += len as i32; // offset of string
                s.to_owned()
              },
              _ => return Err(VMExecError::InvalidOperationContent(self.op_i as usize))
            })));
            let val_point = &*val as *const Value;
            self.root.pool.push(val);
            self.stack_push(val_point);
          },
          PUSH_VAR => {
            unsafe {
              let mut pos = self.get_debug_pos()?;

              let identifier = self.stack_pop();
              let identifier = match &*identifier {
                &Value::Literal(Literal::String(ref s)) => s,
                _ => return Err(VMExecError::InvalidIdentifier(format!("{:?}", &*identifier)))
              };
              let val = Box::new(Value::Variable(identifier.to_string(), pos)); // temp
              let val_point = &*val as *const Value;
              self.root.pool.push(val);
              self.stack_push(val_point);
            }
          },
          ADD | SUB | MULTIPLY | ASSIGN | DIVIDE |
          GT | LT | GTOREQ | LTOREQ => {
            let pos = self.get_debug_pos()?;

            let second = self.stack_pop();
            let first = self.stack_pop();
            let res = self.literal_operation(first, second, code, pos)?;
            self.stack_push(res);
          },
          JUMP => {
            unsafe {
              let to = self.get_int()?;
              // let to = self.stack_pop();
              // let to: f64 = match &*to {
              //   &Value::Literal(Literal::Num(to)) => to as f64,
              //   _ => return Err(VMExecError::Temp)
              // };
              // println!("to: {}", to);
              self.op_i += to;
            }
          },
          JUMPIFN => {
            unsafe {
              let mut expr_pos = match self.get_debug_pos()? {
                Some(pos) => pos,
                None => 0
              };

              // let to = self.stack_pop();
              let to = self.get_int()?;
              let expr = self.stack_pop();
              let expr: bool = match &*expr {
                &Value::Literal(ref literal) => {
                  match *literal {
                    Literal::Bool(b) => b,
                    _ => return Err(VMExecError::InvalidCast(literal.clone(), "String".to_string(), expr_pos))
                  }
                },
                &Value::Variable(ref identifier, _) => {
                  false
                },
                _ => return Err(VMExecError::Temp)
              };

              if !expr {
                // println!("jumping to: {:#?}", vec!(&self.program[self.op_i + to - 2], &self.program[self.op_i + to - 1], &self.program[self.op_i + to]));
                self.op_i += to;
              }
            }
          },
          PRINT => {
            let mut pos = self.get_debug_pos()?;

            unsafe {
              println!("{}", (*self_point).value_to_string(self.stack_peek(), false));
            }
          },
          POP => {
            self.stack_pop();
          }
          _ => return Err(VMExecError::UnsupportedOPCode(format!("{:?}", op)))
        };

        self.op_i += 1;
      } else {
        return Err(VMExecError::InvalidOPCode(format!("{:?}", op.val)))
      }
    }
  }
}