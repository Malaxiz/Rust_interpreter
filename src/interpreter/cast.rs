// fn get_type(literal: *const Literal) {
//   unsafe {
//     match &*literal {
//       &Literal::Num(_) => Ok("num".to_string()),
//       &Literal::String(_) => Ok("string".to_string()),
//       &Literal::Nil => Ok("nil".to_string()),
//       &Literal::Bool(_) => Ok("boolean".to_string()),
//       &Literal::Function(ref parameters, _) => Ok(format!("<function ({:?})>", parameters)),
//       &Literal::Variable(ref name) => match get_var(name as &str) {
//         Some(val) => get_type(get_var, val, pos),
//         None => {
//           println!("{}", pos);
//           Err(InterpreterErr::IdentifierNotFound(String::from(name as &str), pos))
//           }
//       },
//     }
//   }
// }