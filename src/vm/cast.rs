use vm::*;
use vm::exec::{VMExec, VMExecError, Scope, Literal, Value, Function};

// pub type NativeVM<'a> = &'a VMExec;
pub type NativePars = Vec<*const Value>;
pub type NativeScope = *mut Scope;
pub type NativeReturn = Result<Option<Value>, VMExecError>;

pub enum FunctionType<'a> {
  InCode(i32, &'a Vec<String>),
  Native(fn(NativeScope, NativePars) -> NativeReturn),
}

type ValuePointer = *const Value;
type StructType = (i32, Option<i32>);

impl VMExec {
  pub fn not_defined(&self, identifier: &str, pos: Option<i32>) -> Result<String, VMExecError> {
    Err(VMExecError::VariableNotDefined(identifier.to_string(), match pos {
      Some(pos) => pos,
      None => 0
    }))
  }

  fn match_var<'a, T, F>(&self, identifier: &str, scope: *mut Scope, pos: Option<i32>, func: F) -> Result<T, VMExecError>
    where F: Fn(&Self, ValuePointer, Option<i32>) -> Result<T, VMExecError>
  {
    match unsafe {(&*scope)}.get_var(identifier) {
      Some(val) => func(self, val, pos),
      None => return Err(VMExecError::VariableNotDefined(identifier.to_string(), match pos {
        Some(pos) => pos,
        None => 0
      }))
    }
  }

  pub fn cast_bool(&self, val: ValuePointer, expr_pos: Option<i32>) -> Result<bool, VMExecError> {
    let val = unsafe {
      &*val
    };

    let scope = self.scope_stack_peek()?;

    Ok(match val {
      &Value::Literal(ref literal) => {
        match *literal {
          Literal::Bool(b) => b,
          _ => return Err(VMExecError::InvalidCast(val.clone(), "<Bool>".to_string(), expr_pos))
        }
      },
      &Value::Variable(ref identifier, pos) => self.match_var(identifier, scope, expr_pos, &Self::cast_bool)?,
      &Value::Pointer(ref identifier, pos, scope) => self.match_var(identifier, scope, expr_pos, &Self::cast_bool)?,
      _ => {
        return Err(VMExecError::Temp(5));
      }
    })
  }

  pub fn cast_func(&self, val: ValuePointer, expr_pos: Option<i32>) -> Result<FunctionType, VMExecError> {
    let val = unsafe {
      &*val
    };

    let mut scope = self.scope_stack_peek()?;

    Ok(match val {
      &Value::Literal(ref literal) => {
        match *literal {
          Literal::Function(ref func_type) => match func_type {
            &Function::InCode(pos, ref parameters) => FunctionType::InCode(pos, parameters),
            &Function::Native(func) => FunctionType::Native(func)
          },
          _ => return Err(VMExecError::InvalidCast(val.clone(), "<function>".to_string(), expr_pos))
        }
      },
      &Value::Variable(ref identifier, pos) => match unsafe {(&*scope)}.get_var(identifier) { // lifetime issues
        Some(val) => self.cast_func(val, pos)?,
        None => return Err(VMExecError::VariableNotDefined(identifier.to_string(), match pos {
          Some(pos) => pos,
          None => 0
        }))
      },
      &Value::Pointer(ref identifier, pos, scope) => match unsafe {(&*scope)}.get_var(identifier) {
        Some(val) => self.cast_func(val, pos)?,
        None => return Err(VMExecError::VariableNotDefined(identifier.to_string(), match pos {
          Some(pos) => pos,
          None => 0
        }))
      }
      _ => {
        return Err(VMExecError::Temp(5));
      }
    })
  }

  pub fn cast_int(&self, val: ValuePointer, expr_pos: Option<i32>) -> Result<i32, VMExecError> {
    let val = unsafe {
      &*val
    };

    let mut scope = self.scope_stack_peek()?;

    Ok(match val {
      &Value::Literal(ref literal) => {
        match *literal {
          Literal::Int(int) => int,
          _ => return Err(VMExecError::InvalidCast(val.clone(), "<int>".to_string(), expr_pos))
        }
      },
      &Value::Variable(ref identifier, pos) => self.match_var(identifier, scope, expr_pos, &Self::cast_int)?,
      &Value::Pointer(ref identifier, pos, scope) => self.match_var(identifier, scope, expr_pos, &Self::cast_int)?,
      _ => {
        return Err(VMExecError::Temp(5));
      }
    })
  }

  pub fn cast_struct(&self, val: ValuePointer, expr_pos: Option<i32>) -> Result<StructType, VMExecError> {
    let val = unsafe {
      &*val
    };

    let mut scope = self.scope_stack_peek()?;

    Ok(match val {
      &Value::Literal(ref literal) => {
        match *literal {
          Literal::Structure(to, debug) => (to, debug),
          _ => return Err(VMExecError::InvalidCast(val.clone(), "<struct>".to_string(), expr_pos))
        }
      },
      &Value::Variable(ref identifier, pos) => self.match_var(identifier, scope, expr_pos, &Self::cast_struct)?,
      &Value::Pointer(ref identifier, pos, scope) => self.match_var(identifier, scope, expr_pos, &Self::cast_struct)?,
      _ => {
        return Err(VMExecError::Temp(5));
      }
    })
  }

  pub fn cast_instance(&self, val: ValuePointer, expr_pos: Option<i32>) -> Result<*mut Scope, VMExecError> {
    let val = unsafe {
      &*val
    };

    let mut scope = self.scope_stack_peek()?;

    Ok(match val {
      &Value::Instance(ref scope) => {
        *scope
        // match *literal {
        //   Literal::Instance(to, debug) => (to, debug),
        //   _ => return Err(VMExecError::InvalidCast(literal.clone(), "<instance>".to_string(), expr_pos))
        // }
      },
      &Value::Variable(ref identifier, pos) => self.match_var(identifier, scope, expr_pos, &Self::cast_instance)?,
      &Value::Pointer(ref identifier, pos, scope) => self.match_var(identifier, scope, expr_pos, &Self::cast_instance)?,
      _ => {
        return Err(VMExecError::InvalidCast(val.clone(), "<instance>".to_string(), expr_pos));
      }
    })
  }
}