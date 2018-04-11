use vm::*;

use lexer::Literal;

pub use parser::Declaration;
pub use parser::Statement;
pub use parser::Expression;
pub use parser::Primary;

#[derive(Debug)]
pub enum VMBuildError {
  // error, pos
  InvalidExpression(String, i32),
  UnsupportedOperator(Token, i32),
  UnsupportedType(Literal, i32),

  Temp
}

pub struct VMBuild {
  program: Vec<u8>,
  is_debug: bool
}

impl VMBuild {
  pub fn new() -> Self {
    Self {
      program: Vec::new(),
      is_debug: true
    }
  }

  fn build_binary(&mut self, expr: &Expression, pos: i32) -> Result<Vec<u8>, VMBuildError> {
    match expr {
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
          &(Token::Plus, pos) => ADD,
          &(Token::Minus, pos) => SUB,
          &(Token::Asterix, pos) => MULTIPLY,
          &(_, pos) => return Err(VMBuildError::UnsupportedOperator(token.0, pos))
        }));

        let pos = match token {
          &(_, pos) => pos
        };

        // println!("pos: {:?}", pos);

        left.push(pos as u8);
        Ok(left)
      },
      &Expression::Primary(ref literal, pos) => {
        match literal {
          &Primary::Identifier(ref identifier) => {
            // println!("identifier: {}", identifier);
            // let val_pointer = self.save_value(Literal::Variable(identifier.clone()));
            // Ok(val_pointer)
            Ok(vec![u(PUSH_NUM), 0x00])
          },
          &Primary::Literal(ref literal) => {
            // let val_pointer = self.save_value(literal.clone());
            // Ok(val_pointer)
            match literal {
              &lexer::Literal::Num(num) => {
                let bv: [u8; 8] = unsafe {
                  mem::transmute(num)
                };
                let mut bv: Vec<u8> = bv.to_vec();

                let mut v = vec![u(PUSH_NUM)];
                v.append(&mut bv);
                Ok(v)
              },
              &lexer::Literal::Bool(b) => {
                let v = vec![u(PUSH_BOOL), if b {0x01} else {0x00}];
                Ok(v)
              },
              &lexer::Literal::String(ref s) => {
                let mut v = vec![u(PUSH_STRING)];
                let mut s: Vec<u8> = s.clone().into_bytes();
                v.append(&mut s);
                v.push(u(NULL));
                Ok(v)
              },
              _ => return Err(VMBuildError::UnsupportedType(literal.clone(), pos))
            }
          }
        }
      },
      _ => return Err(VMBuildError::InvalidExpression(format!("{:?}", expr), pos))
    }
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

  pub fn build(&mut self, decls: Decls, query: String, options: BuildOptions) -> Result<Program, VMBuildError> {
    // let mut program: Vec<u8> = vec![VERSION as u8, 0x01, DEBUG as u8, META_END as u8,
    //                                 PUSH_INT as u8, 0x01, PUSH_INT as u8, 0x01, SUB as u8, 0x02,
    //                                 END as u8,
    //                                 DEBUG_CODE as u8,
    //                                 '1' as u8, '+' as u8, '1' as u8, ';' as u8,
    //                                 DEBUG_CODE_END as u8];

    // let mut program: Vec<u8> = vec![u(VERSION), 0x01, u(DEBUG), u(META_END)];

    let mut program: Vec<u8> = vec![u(VERSION), 0x01, u(DEBUG), u(DEBUG_CODE)];
    let mut code = query.as_bytes().to_vec();
    program.append(&mut code);
    program.push(u(DEBUG_CODE_END));
    program.push(u(META_END));


    for i in decls {
      // println!("{:?}", i);
      let mut built = self.build_decl(&*i)?;
      program.append(&mut built);
    }
    
    program.push(u(END));

    let mut file = File::create("foo.ops").unwrap();
    file.write_all(&program).unwrap();

    let program: Program = program.iter().map(|c| Operation {
      code: OPCode::from_i32(*c as i32),
      val: *c
    }).collect();

    Ok(program)
  }
}