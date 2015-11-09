open Ast 

let txt_of_op = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mult -> "Mult"
  | Div -> "Div"
  | Mod -> "Mod"
  | Pow -> "Pow"

let rec txt_of_expr = function
  | Int_lit(x) -> "Int_lit(" ^ string_of_int x ^ ")"
  | Float_lit(x) -> "Float_lit(" ^ string_of_float x ^ ")"
  | String_lit(x) -> "String_lit(" ^ x ^ ")"
  | Id(x) -> "Id(" ^ x ^ ")"
  | Unop(op, e1) -> let v1 = txt_of_expr e1 and op1 = txt_of_op op in
    "Unop(" ^ op1 ^ ", " ^ v1 ^ ")"
  | Binop(e1, op, e2) ->
    let v1 = txt_of_expr e1 and op1 = txt_of_op op and v2 = txt_of_expr e2 in
    "Binop(" ^ v1 ^ ", " ^ op1 ^ ", " ^ v2 ^ ")"
  | Call(f, args) -> let args1 = List.map txt_of_expr args in
    "Call(" ^ f ^ ", [" ^ String.concat " ; " args1 ^ "])"

let rec txt_of_stmt = function
  | Expr(expr) -> let e = txt_of_expr expr in "Expr(" ^ e ^ ")"

let rec txt_of_stmts acc = function
  | [] -> "[" ^ (String.concat " ; " acc) ^ "]"
  | stmt :: tl -> txt_of_stmts (txt_of_stmt stmt :: acc) tl

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.program Scanner.token lexbuf in
  let result = txt_of_stmts [] expr in
  print_endline result
