use vm::*;

use lexer::Literal;

pub use parser::{Declaration, Statement, Expression, Primary};

#[derive(Debug)]
pub enum VMBuildError {
  // error, pos
  InvalidExpression(String, i32),
  UnsupportedOperator(Token, i32),
  UnsupportedType(Literal, i32),

  Temp
}

fn get_string_binary(string: &str) -> Vec<u8> {
  let mut string: Vec<u8> = string.as_bytes().to_vec();
  string.push(u(NULL));
  string
}

fn get_num_binary(num: f64) -> Vec<u8> {
  let bv: [u8; 8] = unsafe {
    mem::transmute(num)
  };
  bv.to_vec()
}

fn get_int_binary(int: i32) -> Vec<u8> {
  let bv: [u8; 4] = unsafe {
    mem::transmute(int)
  };
  bv.to_vec()
}

pub struct VMBuild {
  is_debug: bool,
  debug_offset: usize,
  curr_pos: i32
}

impl VMBuild {
  pub fn new() -> Self {
    Self {
      is_debug: false,
      curr_pos: 0,
      debug_offset: 0
    }
  }

  fn reset(&mut self) {
    self.is_debug = false;
    self.curr_pos = 0;
  }

  fn get_debug_binary(&self, pos: i32) -> Vec<u8> {
    get_int_binary(pos + self.debug_offset as i32)
  }

  fn build_binary(&mut self, expr: &Expression, pos: i32) -> Result<Vec<u8>, VMBuildError> {
    let v = match expr {
      &Expression::Binary(ref left, ref token, ref right) => {
        let left_pos = match **left {
          Expression::Primary(_, pos) => pos,
          _ => 0
        };
        let right_pos = match **right {
          Expression::Primary(_, pos) => pos,
          _ => 0
        };

        let mut left = self.build_binary(&*left, left_pos)?;
        let mut right = self.build_binary(&*right, right_pos)?;

        left.append(&mut right);

        left.push(u(match token {
          &(Token::Plus, _) => ADD,
          &(Token::Minus, _) => SUB,
          &(Token::Asterix, _) => MULTIPLY,
          &(Token::Equals, _) => ASSIGN,
          &(Token::Slash, _) => DIVIDE,
          
          &(Token::Lt, _) => LT,
          &(Token::Gt, _) => GT,
          &(Token::LtOrEquals, _) => LTOREQ,
          &(Token::GtOrEquals, _) => GTOREQ,

          &(_, pos) => return Err(VMBuildError::UnsupportedOperator(token.0, pos))
        }));

        if self.is_debug {
          let pos = match token {
            &(_, pos) => pos
          };

          left.push(u(I32)); 
          left.append(&mut self.get_debug_binary(pos));
        }

        left
      },
      &Expression::Primary(ref literal, pos) => {
        match literal {
          &Primary::Identifier(ref identifier) => {
            // let mut v = vec![u(PUSH_STRING)];
            // let mut s: Vec<u8> = identifier.clone().into_bytes();
            // v.append(&mut s);
            // v.push(u(NULL));
            let mut v = vec![];
            v.push(u(PUSH_VAR));

            let mut s: Vec<u8> = identifier.clone().into_bytes();
            v.append(&mut s);
            v.push(u(NULL));

            if self.is_debug {
              v.push(u(I32));
              v.append(&mut self.get_debug_binary(pos));
            }

            v
          },
          &Primary::Literal(ref literal) => {
            match literal {
              &lexer::Literal::Num(num) => {
                let bv: [u8; 8] = unsafe {
                  mem::transmute(num)
                };
                let mut bv: Vec<u8> = bv.to_vec();

                let mut v = vec![u(PUSH_NUM)];
                v.append(&mut bv);
                v
              },
              &lexer::Literal::Bool(b) => {
                let v = vec![u(PUSH_BOOL), if b {0x01} else {0x00}];
                v
              },
              &lexer::Literal::String(ref s) => {
                let mut v = vec![u(PUSH_STRING)];
                v.append(&mut get_string_binary(s));
                v
              },
              &lexer::Literal::Nil => {
                vec![u(PUSH_NIL)]
              },
              _ => return Err(VMBuildError::UnsupportedType(literal.clone(), pos))
            }
          }
        }
      },
      &Expression::PrintExpr(ref expr, pos) => {
        let expr_pos = match **expr {
          Expression::Primary(_, pos) => pos,
          _ => 0
        };

        let mut v = self.build_binary(&*expr, expr_pos)?;
        v.push(u(PRINT));

        if self.is_debug {
          v.push(u(I32)); 
          v.append(&mut self.get_debug_binary(pos));
        }

        v
      },
      &Expression::IfExpr(ref expr, ref body, ref else_body, expr_pos, pos) => {
        let mut body_v = Vec::new();
        let mut body_len = 0;
        for i in body {
          let decl = self.build_decl(i)?;
          body_len += decl.len();
          body_v.push(decl);
        }

        {
          let last_is_expr: bool = body_len <= 0 || match *(*body)[body.len() - 1] {
            Declaration::Statement(ref stmt, _) => match **stmt {
              Statement::ExpressionStmt(_, is_expr, _) => is_expr,
              _ => false // should not happen
            },
            _ => false // should not happen
          };

          if last_is_expr {
            body_v.push(vec![u(PUSH_NIL)]);
            body_len += 1;
          }
        }

        let mut else_v = Vec::new();
        let mut else_len = 0;
        for i in else_body {
          let decl = self.build_decl(i)?;
          else_len += decl.len();
          else_v.push(decl);
        }

        {
          let last_is_expr: bool = else_len <= 0 || match *(*else_body)[else_body.len() - 1] {
            Declaration::Statement(ref stmt, _) => match **stmt {
              Statement::ExpressionStmt(_, is_expr, _) => is_expr,
              _ => false // should not happen
            },
            _ => false // should not happen
          };

          if last_is_expr {
            else_v.push(vec![u(PUSH_NIL)]);
            else_len += 1;
          }
        }

        let mut debug_info = vec![];
        if self.is_debug {
          debug_info.push(u(I32));
          debug_info.append(&mut self.get_debug_binary(expr_pos));
        }

        let mut v = self.build_binary(&*expr, expr_pos)?;
        v.push(u(SCOPE_NEW));

        v.push(u(JUMPIFN));
        // println!("tojump: {}", (body_len as i32) + 5);
        v.append(&mut debug_info);
        v.push(u(I32));
        v.append(&mut get_int_binary((body_len as i32) + 6)); // 6 magic offset = u(JUMP) + u(I32) + 1

        for mut i in body_v {
          v.append(&mut i);
        }

        v.push(u(JUMP));
        v.push(u(I32));
        v.append(&mut get_int_binary(else_len as i32));

        for mut i in else_v {
          v.append(&mut i);
        }

        v.push(u(SCOPE_END));

        v
      },
      &Expression::WhileExpr(ref expr, ref body, expr_pos, pos) => {
        let mut body_v = Vec::new();
        let mut body_len: i32 = 0;
        for i in body {
          let decl = self.build_decl(i)?;
          body_len += decl.len() as i32;
          body_v.push(decl);
        }

        let mut debug_offset = 0;
        let mut debug_info = vec![];
        if self.is_debug {
          debug_info.push(u(I32));
          debug_info.append(&mut self.get_debug_binary(expr_pos));
          debug_offset = 5;
        }

        let last_is_expr: bool = body_len <= 0 || match *(*body)[body.len() - 1] {
          Declaration::Statement(ref stmt, _) => match **stmt {
            Statement::ExpressionStmt(_, is_expr, _) => is_expr,
            _ => false // should not happen
          },
          _ => false // should not happen
        };

        if last_is_expr {
          body_v.push(vec![u(PUSH_NIL)]);
          body_len += 1;
        }

        let mut expr = self.build_binary(&*expr, expr_pos)?;
        let expr_len = expr.len() as i32;

        let mut v: Vec<u8> = Vec::new();

        v.push(u(SCOPE_NEW));
        v.push(u(JUMP));                // 1. Jump
        v.push(u(I32));
        v.append(&mut get_int_binary(expr_len + 1 + 5 + 6 + 11 + 1 + 6 - 5 + 2 + debug_offset));

        v.push(u(SCOPE_BACK));          // 2. Expr
        v.append(&mut expr);
        v.push(u(SCOPE_FORWARD));

        v.push(u(JUMPSTACK));           // 3. JumpStack

        v.push(u(PUSH_JUMP));            // 4. PushInt
        v.append(&mut get_int_binary(5 + 1 + 5));

        v.push(u(JUMP));                // 5. Jump
        v.push(u(I32));
        v.append(&mut get_int_binary(-(5 + 1 + 1 + expr_len + 1 + 5 + 1)));

        v.push(u(JUMPIFN));             // 6. JumpIfNot
        v.append(&mut debug_info.clone());
        v.push(u(I32));
        v.append(&mut get_int_binary(1 + 6 + 5 + 6 + 6 + debug_offset + body_len + 6));

        v.push(u(POP));                 // 7. Pop

        v.push(u(JUMP));                // 8. Jump
        v.push(u(I32));
        v.append(&mut get_int_binary(5 + 6 + 6 + debug_offset));

        v.push(u(PUSH_JUMP));            // 9. PushInt
        v.append(&mut get_int_binary(5 + 6 + 11 + 1 + 6 + 5 + 6 - 3 - 2 + debug_offset));

        v.push(u(JUMP));                // 10. Jump
        v.push(u(I32));
        v.append(&mut get_int_binary(-(5 + 6 + 1 + 11 + 6 + 5 + 1 + expr_len + 1 + 2 + debug_offset)));

        v.push(u(JUMPIFN));             // 11. JumpIfNot
        v.append(&mut debug_info);
        v.push(u(I32));
        v.append(&mut get_int_binary(body_len + 6));

        for mut i in body_v {           // 12. Body
          v.append(&mut i);
        }

        v.push(u(JUMP));                // 13. Jump
        v.push(u(I32));
        v.append(&mut get_int_binary(-(body_len + 5 + debug_offset + 1 + 5 + 1 + 5 + 5 + 1 + 1 + 5 + debug_offset + 1 + 5 + 1 + 5 + 1 + 5)));

        v.push(u(SCOPE_END));

        v
        // println!("{:?}", get_program(v));
        // Ok(vec![u(PUSH_NIL)])
      },
      &Expression::FunctionExpr(ref parameters, ref body, pos) => {
        let mut body_v = Vec::new();
        for i in body {
          body_v.append(&mut self.build_decl(i)?);
        }

        let mut parameter_v = Vec::new();
        for i in parameters {
          parameter_v.push(u(STRING));
          parameter_v.append(&mut get_string_binary(i));
        }

        let last_is_stmt = body.len() <= 0 || match &*body[body.len() - 1] {
          &Declaration::Statement(ref stmt, _) => match &**stmt {
            &Statement::ExpressionStmt(ref _box, is_stmt, _) => is_stmt,
            _ => false
          },
          _ => false
        };

        let mut v = vec![u(PUSH_FUNC)];

        let mut debug_offset = 0;
        if self.is_debug {
          v.push(u(I32));
          v.append(&mut self.get_debug_binary(pos));
          debug_offset = 5;
        }

        v.push(u(I32));
        v.append(&mut get_int_binary(1 + 1 + 4));

        v.push(u(I32));
        v.append(&mut get_int_binary(parameters.len() as i32));

        v.append(&mut parameter_v);

        v.push(u(JUMP));
        v.push(u(I32));
        v.append(&mut get_int_binary(body_v.len() as i32 + 1 + if last_is_stmt {1} else {0}));

        v.append(&mut body_v);
        if last_is_stmt {
          v.push(u(PUSH_NIL))
        }

        v.push(u(JUMPSTACKABS));
        v
      },
      &Expression::FunctionCallExpr(ref expr, ref args, pos) => {
        let mut debug_info = Vec::new();
        if self.is_debug {
          debug_info.push(u(I32));
          debug_info.append(&mut self.get_debug_binary(pos));
        }

        let mut v = Vec::new();

        for i in args {
          v.append(&mut self.build_expr(i, pos)?);
        }

        v.append(&mut self.build_expr(expr, pos)?);

        v.push(u(SCOPE_NEW));

        v.push(u(CALL_FUNC));
        v.append(&mut debug_info);

        v.push(u(I32));
        v.append(&mut get_int_binary(args.len() as i32));

        v.push(u(SCOPE_END));

        v
      },
      _ => return Err(VMBuildError::InvalidExpression(format!("{:?}", expr), pos))
    };

    Ok(v)
  }

  fn build_expr(&mut self, expr: &Expression, pos: i32) -> Result<Vec<u8>, VMBuildError> {
    let binary = self.build_binary(expr, pos);
    binary
  }

  fn build_stmt(&mut self, stmt: &Statement) -> Result<Vec<u8>, VMBuildError> {
    match stmt {
      &Statement::ExpressionStmt(ref expr, is_statement, pos) => {
        let mut res = self.build_expr(expr, pos)?;
        if is_statement {
          res.push(u(POP));
        }
        Ok(res)
      }
    }
  }

  fn build_decl(&mut self, decl: &Declaration) -> Result<Vec<u8>, VMBuildError> {
    match decl {
      &Declaration::Statement(ref stmt, pos) => self.build_stmt(stmt)
    }
  }

  pub fn build(&mut self, decls: Decls, query: String, debug_offset: usize, options: BuildOptions) -> Result<Instructions, VMBuildError> {
    self.reset();
    self.debug_offset = debug_offset;

    let mut program: Vec<u8> = vec![u(VERSION), 0x01];

    if options.contains(BuildOptions::DEBUG) {
      self.is_debug = true;
      program.push(u(DEBUG))
    }

    if options.contains(BuildOptions::CODE) {
      program.push(u(DEBUG_CODE));
      let mut code = query.as_bytes().to_vec();
      program.append(&mut code);
      program.push(u(DEBUG_CODE_END));
    }

    program.push(u(META_END));

    for i in decls {
      self.curr_pos = program.len() as i32;
      let mut built = self.build_decl(&*i)?;
      program.append(&mut built);
    }
    
    program.push(u(END));

    Ok(program)
  }
}