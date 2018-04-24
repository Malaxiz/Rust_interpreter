use vm::*;
use vm::OPCode::*;
use std::collections::HashMap;
use self::cast::{FunctionType, NativeScope, NativePars, NativeReturn};

use self::native::{value_to_string, add_func, input_func, print_func, format_func};

const STACK_SIZE: usize = 512;

#[derive(Debug)]
pub enum VMExecError {
  // error, pos
  UnsupportedOperation(Literal, Literal, OPCode, i32),
  UnsupportedValueOperation(Value, Value, OPCode, i32),
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
  InvalidCast(Value, String, Option<i32>),

  Temp(i32)
}

#[derive(Clone, Debug)]
pub enum Literal {
  Num(f64),
  Int(i32),
  Bool(bool),
  String(String),
  Function(Function),

  // op_i pos, debug_pos
  Structure(i32, Option<i32>),

  Nil
}

#[derive(Clone, Debug)]
pub enum Value {
  Variable(String, Option<i32>),
  Pointer(String, Option<i32>, *mut Scope),
  Instance(*mut Scope),
  // Reference(*const Value),
  Literal(Literal),
  None,
}

#[derive(Clone, Debug)]
pub enum Function {
  Native(fn(NativeScope, NativePars) -> NativeReturn),

  // op_ipos , parameters
  InCode(i32, Vec<String>)
}

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
    let scope_point = &mut *scope as *mut Scope;
    this.root.scopes.push(scope);
    this.scope_stack[0] = Some(scope_point);

    let mut funcs: HashMap<&'static str, fn(NativeScope, NativePars) -> NativeReturn> = HashMap::new();
    funcs.insert("add", add_func);
    funcs.insert("input", input_func);
    funcs.insert("print", print_func);
    funcs.insert("format", format_func);

    for (k, i) in funcs {
      let func = Box::new(Value::Literal(Literal::Function(Function::Native(i))));
      let func_point: *const Value = &*func;
      this.root.pool.push(func);
      unsafe {
        (*scope_point).set_var_directly(k, func_point);
      }
    }

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

  fn literal_operation(&mut self, val1f: *const Value, val2f: *const Value, operation: &OPCode, pos: Option<i32>) -> Result<*const Value, VMExecError> {
    let get_pos = || {
      match pos {
        Some(pos) => pos,
        _ => -1
      }
    };

    let keep_vars = match operation {
      &ASSIGN | &LET | &DOT => true,
      _ => false
    };

    let mut val1 = val1f;
    let mut val2 = val2f;

    let mut scope = unsafe {
      &*self.scope_stack_peek()?
    };

    unsafe {
      if !keep_vars {
        if let &Value::Variable(ref identifier, pos) = &*val1f {
          val1 = match scope.get_var(identifier) {
            Some(val) => val,
            None => return Err(VMExecError::VariableNotDefined(identifier.to_string(), match pos {
              Some(pos) => pos,
              None => 0
            }))
          }
        } else if let &Value::Pointer(ref identifier, pos, scope) = &*val1f {
          val1 = match (&*scope).get_var(identifier) {
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
      } else if let &Value::Pointer(ref identifier, pos, scope) = &*val2f {
        val2 = match (&*scope).get_var(identifier) {
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
        (&Value::Variable(ref identifier, ref pos), _) => {
          match operation {
            &ASSIGN => {
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
              Ok(val2)
            },
            &LET => {
              let mut scope = unsafe {
                &mut *self.scope_stack_peek()?
              };
              scope.set_var_directly(identifier, val2);
              Ok(val2)
            }
            _ => {
              Ok(NIL)
            }
          }
        },
        (&Value::Pointer(ref identifier, ref pos, scope), _) => {
          match operation {
            &ASSIGN => {
              let mut scope = unsafe {
                &mut *scope
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
              Ok(val2)
            },
            &LET => {
              let mut scope = unsafe {
                &mut *scope
              };
              scope.set_var_directly(identifier, val2);
              Ok(val2)
            }
            _ => {
              Ok(NIL)
            }
          }
        },
        _ => return Err(VMExecError::UnsupportedValueOperation((&*val1).clone(), (&*val2).clone(), operation.clone(), get_pos()))
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
    let self_point: *mut Self = self;

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
        //   let cont = format!("{:#?}", code);
        //   let mut repeat: i32 = 14 - cont.len() as i32;
        //   repeat = if repeat < 0 {0} else {repeat};
        //   println!("code: {}{} | {:?}", cont, " ".repeat(repeat as usize), content);
          // self.print_stack();
        }
        
        match *code {
          END => {
            unsafe {
              return Ok(value_to_string((*self_point).scope_stack_peek()?, self.stack_pop(), true)?);
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
            let val = match content {
              &OperationLiteral::Int(int) => int,
              _ => return Err(VMExecError::InvalidOperationContent(self.op_i as usize))
            };
            self.op_i += 4; // offset of i32
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
          },
          PUSH_POINTER => {
            let lookup = self.get_string()?;
            let pos = self.get_debug_pos()?;

            let val = Box::new(Value::Pointer(lookup.to_string(), pos, self.scope_stack_peek()?)); // temp
            let val_point = &*val as *const Value;
            self.root.pool.push(val);
            self.stack_push(val_point);
          },
          PUSH_VALUE | PUSH_VALUE_DIRECT => {
            let value = self.stack_pop();
          
            match unsafe { &*value } {
              &Value::Variable(ref identifier, _) => {
                let mut scope = unsafe {
                  &mut *self.scope_stack_peek()?
                };

                let val = if *code == PUSH_VALUE_DIRECT {
                  scope.get_var_directly(identifier)
                } else {
                  scope.get_var(identifier)
                };

                match val {
                  Some(val) => self.stack_push(val),
                  None => self.stack_push(NIL)
                }
              },
              &Value::Pointer(ref identifier, _, scope) => {
                let mut scope = unsafe {
                  &mut *scope
                };

                let val = if *code == PUSH_VALUE_DIRECT {
                  scope.get_var_directly(identifier)
                } else {
                  scope.get_var(identifier)
                };

                match val {
                  Some(val) => self.stack_push(val),
                  None => self.stack_push(NIL)
                }
              }
              _ => self.stack_push(value)
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

            let val = Box::new(Value::Literal(Literal::Function(Function::InCode(self.op_i + body_offset, parameters))));
            let val_point = &*val as *const Value;
            self.root.pool.push(val);
            self.stack_push(val_point);
          },
          CALL_FUNC | CALL_FUNC_STACK_ARGS => {
            let self_point = self as *mut Self;

            let pos = self.get_debug_pos()?;

            let func_val = self.stack_pop();
            let func = unsafe {
              (*self_point).cast_func(func_val, pos)?
            };

            let args_len = if *code == CALL_FUNC {
              self.get_int()? as usize
            } else {
              let val = self.stack_pop();
              self.cast_int(val, pos)? as usize
            };

            let mut args = Vec::with_capacity(args_len as usize);
            for _ in 0..args_len {
              args.push(self.stack_pop());
            }
            args = args.into_iter().rev().collect();

            match func {
              FunctionType::InCode(to, func_pars) => {
                let func_pars_len = func_pars.len();

                if true { // strict function mode
                  if func_pars_len != args_len {
                    return Err(VMExecError::ArgumentMismatch(func_pars.clone(), func_pars_len as i32, args_len as i32, to, match pos {
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

                for (k, v) in func_pars.into_iter().enumerate() {
                  scope.set_var_directly(v, args[k]);
                }

                let jump_stack = self.op_i;
                self.jump_stack_push(jump_stack);

                self.op_i = to;
              },
              FunctionType::Native(func) => {
                let res = func(self.scope_stack_peek()?, args)?;
                match res {
                  Some(val) => {
                    let val = Box::new(val); // temp
                    let val_point = &*val as *const Value;
                    self.root.pool.push(val);
                    self.stack_push(val_point);
                  },
                  None => self.stack_push(NIL)
                }
              }
            }
          },
          CALL_STRUCT => {
            let pos = self.get_debug_pos()?;

            let s = self.stack_pop();
            let s = self.cast_struct(s, pos)?;

            let jump_stack = self.op_i;
            self.jump_stack_push(jump_stack);

            self.op_i = s.0;
          },
          PUSH_STRUCT => {
            let pos = self.get_debug_pos()?;
            let to = self.get_int()?;

            let val = Value::Literal(Literal::Structure(to + self.op_i, pos));
            let val = Box::new(val); // temp
            let val_point = &*val as *const Value;
            self.root.pool.push(val);
            self.stack_push(val_point);

            // self.stack_push(NIL);
          },
          GET_SCOPE => {
            let pos = self.get_debug_pos()?;

            let scope_val = self.stack_pop();
            let scope = self.cast_instance(scope_val, pos)?;
            self.scope_stack_push(scope, pos)?;

            let scope = unsafe {
              &mut *scope
            };

            scope.set_var_directly("self", scope_val);
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
          // DOT => {
          //   let pos = self.get_debug_pos()?;

          //   let child = self.stack_pop();
          //   let parent = self.stack_pop();


          // },
          SCOPE_NEW | SCOPE_NEW_FUNC => {
            let parent = match unsafe { &*self.stack_peek() } {
              &Value::Pointer(_, _, scope) => scope,
              _ => self.scope_stack_peek()?
            };

            let mut scope = Box::new(Scope::new(&mut self.root as *mut Root, Some(parent)));
            let mut scope_point = &mut *scope as *mut Scope;
            self.root.scopes.push(scope);
            self.scope_stack_push(scope_point, None)?;


            // let mut scope = Box::new(Scope::new(&mut self.root as *mut Root, Some(self.scope_stack_peek()?)));
            // let mut scope_point = &mut *scope as *mut Scope;
            // self.root.scopes.push(scope);
            // self.scope_stack_push(scope_point, None)?;
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
          SCOPE_PUSH => {
            let scope = self.scope_stack_pop()?;
            let val = Value::Instance(scope);
            let val = Box::new(val);
            let val_point = &*val as *const Value;
            self.root.pool.push(val);
            self.stack_push(val_point);
          },
          JUMP => {
            let to = self.get_int()?;
            // println!("jumplength: {}", to);
            // println!("jumping to: {:#?}", vec!(&self.program[(self.op_i + to - 1) as usize], &self.program[(self.op_i + to) as usize], &self.program[(self.op_i + to + 1) as usize]));
            self.op_i += to;
          },
          JUMPSTACK => {
            let to = self.jump_stack_pop()?;
            self.op_i += to;
          },
          JUMPSTACKABS => {
            let to = self.jump_stack_pop()?;
            self.op_i = to;
          },
          JUMPIFN => {
            let mut expr_pos = self.get_debug_pos()?;

            // let to = self.stack_pop();
            let to = self.get_int()?;
            let expr = self.stack_pop();
            let expr = self.cast_bool(expr, expr_pos)?;

            if !expr {
              // println!("jumping to: {:#?}", vec!(&self.program[self.op_i + to - 2], &self.program[self.op_i + to - 1], &self.program[self.op_i + to]));
              self.op_i += to;
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

  pub fn exec(&mut self, program: Program, append: bool) -> Result<String, VMExecError> {
    match self.do_exec(program, append) {
      Ok(val) => Ok(val),
      Err(err) => {
        self.op_i = self.program.len() as i32;
        Err(err)
      }
    }
  }
}