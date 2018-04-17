

use vm::*;
use vm::exec::{VMExec, Literal, Value};

type ValuePointer = *const Value;

impl VMExec {
  pub fn not_defined(&self, identifier: &str, pos: Option<i32>) -> Result<String, VMExecError> {
    Err(VMExecError::VariableNotDefined(identifier.to_string(), match pos {
      Some(pos) => pos,
      None => 0
    }))
  }

  pub fn cast_bool(&self, val: ValuePointer, expr_pos: Option<i32>) -> Result<bool, VMExecError> {
    let val = unsafe {
      &*val
    };

    let mut scope = unsafe {
      &*self.scope_stack_peek()?
    };

    let expr_pos = match expr_pos {
      Some(pos) => pos,
      None => 0
    };

    Ok(match val {
      &Value::Literal(ref literal) => {
        match *literal {
          Literal::Bool(b) => b,
          _ => return Err(VMExecError::InvalidCast(literal.clone(), "<Bool>".to_string(), expr_pos))
        }
      },
      &Value::Variable(ref identifier, pos) => match scope.get_var(identifier) {
        Some(val) => self.cast_bool(val, Some(expr_pos))?,
        None => return Err(VMExecError::VariableNotDefined(identifier.to_string(), match pos {
          Some(pos) => pos,
          None => 0
        }))
      },
      _ => {
        return Err(VMExecError::Temp(5));
      }
    })
  }

  pub fn cast_func(&self, val: ValuePointer, expr_pos: Option<i32>) -> Result<(i32, &Vec<String>), VMExecError> {
    let val = unsafe {
      &*val
    };

    let mut scope = unsafe {
      &*self.scope_stack_peek()?
    };

    let expr_pos = match expr_pos {
      Some(pos) => pos,
      None => 0
    };

    Ok(match val {
      &Value::Literal(ref literal) => {
        match *literal {
          Literal::Function(pos, ref parameters) => (pos, parameters),
          _ => return Err(VMExecError::InvalidCast(literal.clone(), "<Function>".to_string(), expr_pos))
        }
      },
      &Value::Variable(ref identifier, pos) => match scope.get_var(identifier) {
        Some(val) => self.cast_func(val, Some(expr_pos))?,
        None => return Err(VMExecError::VariableNotDefined(identifier.to_string(), match pos {
          Some(pos) => pos,
          None => 0
        }))
      },
      _ => {
        return Err(VMExecError::Temp(5));
      }
    })
  }
}