use vm::*;
use vm::OPCode::*;
use enum_primitive::FromPrimitive;
use std::collections::HashMap;

const STACK_SIZE: usize = 512;

#[derive(Debug)]
pub enum VMExecError {
  // error, pos
  UnsupportedOperation(Literal, Literal, OPCode, i32),
  InvalidOPCode(String),
  UnsupportedOPCode(String),
  VariableNotDefined(String, i32),

  // STACK_LIMIT, pos
  StackLimitReached(i32, i32),

  // func_pars, func_pars_len, args_len, func_abs_pos, pos
  ArgumentMismatch(Vec<String>, i32, i32, i32, i32),

  // position in bytecode
  InvalidOperationContent(usize),
  InvalidIdentifier(String),
  InvalidCast(Literal, String, i32),

  Temp(i32)
}

#[derive(Clone, Debug)]
pub enum Literal {
  Num(f64),
  Int(i32),
  Bool(bool),
  String(String),

  // pos, parameters
  Function(i32, Vec<String>),

  Nil
}

#[derive(Clone, Debug)]
pub enum Value {
  Variable(String, Option<i32>),
  Literal(Literal),
  None,
}

// fn get_binary_pos(Vec<u8>)

pub struct Root {
  pool: Vec<Box<Value>>,
  scopes: Vec<Box<Scope>>
}

impl Root {
  pub fn new() -> Self {
    Self {
      pool: Vec::new(),
      scopes: Vec::new()
    }
  }

  pub fn gc() {
    // todo
  }
}

pub struct Scope {
  root: *mut Root,
  parent: Option<*mut Scope>,
  variables: HashMap<String, *const Value>,
}

impl Scope {
  pub fn new(root: *mut Root, parent: Option<*mut Scope>) -> Self {
    Self {
      root,
      parent,
      variables: HashMap::new()
    }
  }

  fn get_var_directly(&self, identifier: &str) -> Option<*const Value> {
    match self.variables.get(identifier) {
      Some(val) => Some(*val),
      None => None
    }
  }

  pub fn get_var(&self, identifier: &str) -> Option<*const Value> {
    match self.variables.get(identifier) {
      Some(val) => Some(*val),
      None => match self.parent {
        Some(parent) => unsafe {
          (*parent).get_var(identifier)
        },
        None => None
      }
    }
  }
  
  fn set_var(&mut self, identifier: &str, val: *const Value) -> bool { // -> did set variable
    match self.get_var_directly(identifier) {
      Some(_) => {
        self.variables.insert(identifier.to_string(), val);
        true
      },
      None => match self.parent {
        Some(parent) => unsafe {
          if (&mut *parent).set_var(identifier, val) {
            true
          } else {
            match self.get_var_directly(identifier) {
              Some(_) => {
                self.variables.insert(identifier.to_string(), val);
                true
              },
              None => false
            }
          }
        },
        None => {
          self.variables.insert(identifier.to_string(), val);
          true
        }
      }
    }
  }

  // pub fn set_var(&mut self, identifier: &str, val: *const Value) {
  //   if !self.find_and_set(identifier, val) {
  //     self.variables.insert(identifier.to_string(), val);
  //   }
  // }

  pub fn set_var_directly(&mut self, identifier: &str, val: *const Value) {
    self.variables.insert(identifier.to_string(), val);
  }
}

pub struct VMExec {
  op_i: i32,
  program: Vec<Operation>,

  // scopes

  // variables: HashMap<String, *const Value>,
  root: Root,

  stack: [*const Value; 512],
  stacki: usize,
  jump_stack: [i32; 512],
  jump_stacki: usize,
  scope_stack: [Option<*mut Scope>; 512],
  scope_stacki: usize,

  pub query: String,

  pub is_debug: bool,
  pub contains_code: bool
}

impl VMExec {
  pub fn new() -> Self {
    let root = Root::new();
    
    let mut this = Self {
      op_i: 0,
      program: Vec::new(),

      // variables: HashMap::new(),
      root,

      stack: [&Value::None; 512],
      stacki: 0,
      jump_stack: [0; 512],
      jump_stacki: 0,
      scope_stack: [None; 512],
      scope_stacki: 0,

      query: String::from(""),

      is_debug: false,
      contains_code: false,
    };

    let mut scope = Box::new(Scope::new(&mut this.root as *mut Root, None));
    let mut scope_point = &mut *scope as *mut Scope;
    this.root.scopes.push(scope);
    this.scope_stack[0] = Some(scope_point);

    this
  }

  fn reset(&mut self, append: bool) {
    if !append {
      self.program = Vec::new();
      self.op_i = 0;
      self.query = String::from("");
    }
    self.stack = [&Value::None; 512];
    self.stacki = 0;
    self.jump_stack = [0; 512];
    self.jump_stacki = 0;
    self.scope_stacki = 0;

    let mut scope = self.scope_stack[0];
    self.scope_stack = [None; 512];
    self.scope_stack[0] = scope;
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

  fn jump_stack_push(&mut self, val: i32) {
    self.jump_stack[self.jump_stacki] = val;
    self.jump_stacki += 1;
  }

  fn jump_stack_pop(&mut self) -> Result<i32, VMExecError> {
    if self.jump_stacki <= 0 {
      return Err(VMExecError::Temp(69));
    }
    self.jump_stacki -= 1;
    Ok(self.jump_stack[self.jump_stacki])
  }

  pub fn scope_stack_peek(&self) -> Result<*mut Scope, VMExecError> {
    // println!("scopei: {}, {:?}", self.scope_stacki, self.scope_stack[self.scope_stacki]);

    match self.scope_stack[self.scope_stacki] {
      Some(val) => Ok(val),
      None => Err(VMExecError::Temp(0))
    }
  }

  fn scope_stack_push(&mut self, val: *mut Scope, pos: Option<i32>) -> Result<(), VMExecError> {
    self.scope_stacki += 1;

    if self.scope_stacki < STACK_SIZE - 1 {
      self.scope_stack[self.scope_stacki] = Some(val);
      Ok(())
    } else {
      Err(VMExecError::StackLimitReached(STACK_SIZE as i32, match pos {
        Some(val) => val,
        None => 0
      }))
    }
  }

  fn scope_stack_pop(&mut self) -> Result<*mut Scope, VMExecError> {
    if self.scope_stacki <= 0 {
      return Err(VMExecError::Temp(1));
    }
    let to_return = self.scope_stack[self.scope_stacki];
    self.scope_stacki -= 1;
    match to_return {
      Some(val) => Ok(val),
      None => Err(VMExecError::Temp(2))
    }
  }

  fn get_string(&mut self) -> Result<String, VMExecError> {
    let self_point: *mut Self = self;
    let string = self.consume_op();
    match string.content {
      OperationLiteral::String(ref s, len) => {
        unsafe {
          (*self_point).op_i += len as i32; // offset of string
        }
        Ok(s.to_owned())
      },
      _ => unsafe { return Err(VMExecError::InvalidOperationContent((*self_point).op_i as usize)) }
    }
  }

  fn get_int(&mut self) -> Result<i32, VMExecError> {
    let self_point: *mut Self = self;
    let int = self.consume_op();
    match int.content {
      OperationLiteral::Int(pos) => {
        unsafe {
          (*self_point).op_i += 4;
        }
        Ok(pos)
      },
      _ => unsafe { return Err(VMExecError::InvalidOperationContent((*self_point).op_i as usize)) }
    }
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
      &Literal::Function(pos, ref arguments) => {
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
      _ => format!("unknown literal (0)")
    }
  }

  fn value_to_string(&self, val: *const Value, quotes: bool) -> Result<String, VMExecError> {
    unsafe {
      let mut scope = {
        &*self.scope_stack_peek()?
      };
      Ok(match *val {
        Value::Literal(ref val) => self.literal_to_string(val, quotes),
        Value::Variable(ref identifier, pos) => match scope.get_var(identifier) {
          Some(val) => match *val {
            Value::Literal(ref val) => self.literal_to_string(val, quotes),
            _ => format!("unknown value (1)")
          },
          None => return Err(VMExecError::VariableNotDefined(identifier.to_string(), match pos {
            Some(pos) => pos,
            None => 0
          }))
        },
        _ => format!("unknown value (2)")
      })
    }
  }

  fn literal_operation(&mut self, val1f: *const Value, val2f: *const Value, operation: &OPCode, pos: Option<i32>) -> Result<*const Value, VMExecError> {
    let get_pos = || {
      match pos {
        Some(pos) => pos,
        _ => -1
      }
    };

    let is_assignment = operation == &ASSIGN || operation == &LET;

    let mut val1 = val1f;
    let mut val2 = val2f;

    let mut scope = unsafe {
      &*self.scope_stack_peek()?
    };

    unsafe {
      if !is_assignment {
        if let &Value::Variable(ref identifier, pos) = &*val1f {
          val1 = match scope.get_var(identifier) {
            Some(val) => val,
            None => return Err(VMExecError::VariableNotDefined(identifier.to_string(), match pos {
              Some(pos) => pos,
              None => 0
            }))
          }
        }
      }
      if let &Value::Variable(ref identifier, pos) = &*val2f {
        val2 = match scope.get_var(identifier) {
          Some(val) => val,
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
        (&Value::Variable(ref identifier, ref pos), &Value::Literal(ref lit)) => {
          match (lit, operation) {
            (_, &ASSIGN) => {
              let mut scope = unsafe {
                &mut *self.scope_stack_peek()?
              };
              if true { // variable strict mode
                match scope.get_var(identifier) {
                  Some(_) => {},
                  None => return Err(VMExecError::VariableNotDefined(identifier.to_string(), match pos {
                    &Some(pos) => pos,
                    &None => 0
                  }))
                }
              }
              scope.set_var(identifier, val2);
              Ok(val2) // might be memory error
            },
            (_, &LET) => {
              let mut scope = unsafe {
                &mut *self.scope_stack_peek()?
              };
              scope.set_var_directly(identifier, val2);
              Ok(val2) // might be memory error
            }
            _ => {
              Ok(NIL)
            }
          }
        },
        _ => return Err(VMExecError::Temp(3))
      }
    }
  }

  fn print_stack(&self) {
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

  fn do_exec(&mut self, mut program: Program, append: bool) -> Result<String, VMExecError> {
    self.reset(append);

    if append {
      self.program.append(&mut program);
    } else {
      self.program = program;
    }

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

              if append {
                self.query += query;
              } else {
                self.query = query.to_string();
              }
            },
            _ => {}
          }
          
          self.op_i += 1;
          continue;
        }

        {
          // let cont = format!("{:#?}", code);
          // let mut repeat: i32 = 14 - cont.len() as i32;
          // repeat = if repeat < 0 {0} else {repeat};
          // println!("code: {}{} | {:?}", cont, " ".repeat(repeat as usize), content);
          // self.print_stack();
        }
        
        match *code {
          END => {
            unsafe {
              return Ok((*self_point).value_to_string(self.stack_pop(), true)?);
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
          PUSH_INT => {
            let val = Box::new(Value::Literal(Literal::Int(match content {
              &OperationLiteral::Int(int) => int,
              _ => return Err(VMExecError::InvalidOperationContent(self.op_i as usize))
            })));
            self.op_i += 4; // offset of i32
            let val_point = &*val as *const Value;
            self.root.pool.push(val);
            self.stack_push(val_point);
          },
          PUSH_JUMP => {
            // let val = Box::new(Value::Literal(Literal::Int(match content {
            //   &OperationLiteral::Int(int) => int,
            //   _ => return Err(VMExecError::InvalidOperationContent(self.op_i as usize))
            // })));
            let val = match content {
              &OperationLiteral::Int(int) => int,
              _ => return Err(VMExecError::InvalidOperationContent(self.op_i as usize))
            };
            self.op_i += 4; // offset of i32
            // let val_point = &*val as *const Value;
            // self.root.pool.push(val);
            // self.jump_stack_push(val_point);
            self.jump_stack_push(val);
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
              // let identifier = self.stack_pop();
              // let identifier = match &*identifier {
              //   &Value::Literal(Literal::String(ref s)) => s,
              //   _ => return Err(VMExecError::InvalidIdentifier(format!("{:?}", &*identifier)))
              // };

              let identifier = match content {
                &OperationLiteral::String(ref s, len) => {
                  self.op_i += len as i32; // offset of string
                  s.to_owned()
                },
                _ => return Err(VMExecError::InvalidOperationContent(self.op_i as usize))
              };

              let mut pos = self.get_debug_pos()?;

              let val = Box::new(Value::Variable(identifier.to_string(), pos)); // temp
              let val_point = &*val as *const Value;
              self.root.pool.push(val);
              self.stack_push(val_point);
            }
          },
          PUSH_FUNC => {
            let pos = self.get_debug_pos()?;
            let body_offset = self.get_int()?;
            let par_len = self.get_int()?;

            let mut parameters = Vec::with_capacity(par_len as usize);
            for i in 0..par_len {
              parameters.push(self.get_string()?);
            }

            let val = Box::new(Value::Literal(Literal::Function(self.op_i + body_offset, parameters)));
            let val_point = &*val as *const Value;
            self.root.pool.push(val);
            self.stack_push(val_point);
          },
          CALL_FUNC => {
            let self_point = self as *mut Self;

            let pos = self.get_debug_pos()?;
            let args_len = self.get_int()? as usize;

            let func = self.stack_pop();
            let func = unsafe {
              (*self_point).cast_func(func, pos)?
            };
            let func_pars_len = func.1.len();

            let mut args = Vec::with_capacity(args_len as usize);
            for i in 0..args_len {
              args.push(self.stack_pop());
            }

            if true { // strict function mode
              if func_pars_len != args_len {
                return Err(VMExecError::ArgumentMismatch(func.1.clone(), func_pars_len as i32, args_len as i32, func.0, match pos {
                  Some(pos) => pos,
                  None => 0
                }));
              }
            } else if func_pars_len > args_len {
              for i in 0..(func_pars_len - args_len) {
                args.push(NIL);
              }
            }

            let mut scope = unsafe {
              &mut *self.scope_stack_peek()?
            };

            for (k, v) in func.1.into_iter().enumerate() {
              scope.set_var_directly(v, args[k]);
            }

            let jump_stack = self.op_i;
            self.jump_stack_push(jump_stack);

            self.op_i = func.0;

            // println!("{:?}", func);
          },
          PUSH_NIL => {
            self.stack_push(NIL);
          },
          ADD | SUB | MULTIPLY | ASSIGN | LET | DIVIDE |
          GT | LT | GTOREQ | LTOREQ => {
            let pos = self.get_debug_pos()?;

            let second = self.stack_pop();
            let first = self.stack_pop();
            let res = self.literal_operation(first, second, code, pos)?;
            self.stack_push(res);
          },
          SCOPE_NEW => {
            let mut scope = Box::new(Scope::new(&mut self.root as *mut Root, Some(self.scope_stack_peek()?)));
            let mut scope_point = &mut *scope as *mut Scope;
            self.root.scopes.push(scope);
            self.scope_stack_push(scope_point, None)?;
          },
          SCOPE_END => {
            self.scope_stack_pop()?;
          },
          SCOPE_FORWARD => {
            self.scope_stacki += 1;
          },
          SCOPE_BACK => {
            self.scope_stacki -= 1;
          },
          JUMP => {
            unsafe {
              let to = self.get_int()?;
              // println!("jumplength: {}", to);
              // println!("jumping to: {:#?}", vec!(&self.program[(self.op_i + to - 1) as usize], &self.program[(self.op_i + to) as usize], &self.program[(self.op_i + to + 1) as usize]));
              self.op_i += to;
            }
          },
          JUMPSTACK => {
            let to = self.jump_stack_pop()?;
            // println!("jumplength: {}", to);
            // println!("jumping to: {:#?}", vec!(&self.program[(self.op_i + to - 1) as usize], &self.program[(self.op_i + to) as usize], &self.program[(self.op_i + to + 1) as usize]));
            self.op_i += to;
          },
          JUMPSTACKABS => {
            let to = self.jump_stack_pop()?;
            // println!("jumplength: {}", to);
            // println!("jumping to: {:#?}", vec!(&self.program[(self.op_i + to - 1) as usize], &self.program[(self.op_i + to) as usize], &self.program[(self.op_i + to + 1) as usize]));
            self.op_i = to;
          },
          JUMPIFN => {
            unsafe {
              let mut expr_pos = self.get_debug_pos()?;

              // let to = self.stack_pop();
              let to = self.get_int()?;
              let expr = self.stack_pop();
              let expr = self.cast_bool(expr, expr_pos)?;

              if !expr {
                // println!("jumping to: {:#?}", vec!(&self.program[self.op_i + to - 2], &self.program[self.op_i + to - 1], &self.program[self.op_i + to]));
                self.op_i += to;
              }
            }
          },
          PRINT => {
            let mut pos = self.get_debug_pos()?;

            unsafe {
              println!("{}", (*self_point).value_to_string(self.stack_peek(), false)?);
            }
          },
          POP => {
            let val = self.stack_pop();

            if self.is_debug {
              unsafe {
                match &*val {
                  &Value::Variable(ref identifier, pos) => {
                    let mut scope = &mut *self.scope_stack_peek()?;
                    match scope.get_var(identifier) {
                      None => return Err(VMExecError::VariableNotDefined(identifier.to_string(), match pos {
                        Some(pos) => pos,
                        None => 0
                      })),
                      _ => {}
                    }
                  },
                  _ => {}
                }
              }
            }
          }
          _ => {
            self.print_stack();
            return Err(VMExecError::UnsupportedOPCode(format!("{:?}", op)));
          }
        };

        self.op_i += 1;
      } else {
        return Err(VMExecError::InvalidOPCode(format!("{:?}", op.val)))
      }
    }
  }

  pub fn exec(&mut self, mut program: Program, append: bool) -> Result<String, VMExecError> {
    match self.do_exec(program, append) {
      Ok(val) => Ok(val),
      Err(err) => {
        self.op_i = self.program.len() as i32;
        Err(err)
      }
    }
  }
}