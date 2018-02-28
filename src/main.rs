extern crate lang;

fn main() {
  // let query = "let print = \"a st\\\"ring\"; if \"dab\".contains(\"d\") {  }";
  // let query = " a >= \"dab\"*what;";
  // let query = "let dab = 55* 84 + 75 / 2 >= \"dab\"";
  let query = "this.fn();";
  let result = lang::exec(query).unwrap();

  // let val = " a = ";
  // println!("{}, {}, {}", &val[2..val.len() - 0], 2, val.len() - 0);
}