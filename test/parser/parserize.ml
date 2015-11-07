open Ast 

let rec eval = function
  | Int_lit(x) -> "Int_lit(" ^ string_of_int x ^ ")"
  | Float_lit(x) -> "Float_lit(" ^ string_of_float x ^ ")"
  | String_lit(x) -> "String_lit(" ^ x ^ ")"
  | Unop(op, e1) ->
    let v1 = eval e1 in "Unop(Sub, " ^ v1 ^ ")"
  | Binop(e1, op, e2) ->
    let v1 = eval e1 and v2 = eval e2 in match op with
      | Add -> "Binop(" ^ v1 ^ ", Add, " ^ v2 ^ ")"
      | Sub -> "Binop(" ^ v1 ^ ", Sub, " ^ v2 ^ ")"
      | Mult -> "Binop(" ^ v1 ^ ", Mult, " ^ v2 ^ ")"
      | Div -> "Binop(" ^ v1 ^ ", Div, " ^ v2 ^ ")"
      | Mod -> "Binop(" ^ v1 ^ ", Mod, " ^ v2 ^ ")"
      | Pow -> "Binop(" ^ v1 ^ ", Pow, " ^ v2 ^ ")"

let rec eval_stmt = function
  | State(expr) -> let e = eval expr in "State(" ^ e ^ ")"

let rec eval_stmts acc = function
  | [] -> "[" ^ (String.concat " ; " acc) ^ "]"
  | stmt :: tl -> eval_stmts (eval_stmt stmt :: acc) tl

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.program Scanner.token lexbuf in
  let result = eval_stmts [] expr in
  print_endline result
