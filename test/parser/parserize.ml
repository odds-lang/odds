open Ast 

let txt_of_op = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mult -> "Mult"
  | Div -> "Div"
  | Mod -> "Mod"
  | Pow -> "Pow"
  | Not -> "Not"
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Less -> "Less"
  | Leq -> "Leq"
  | Greater -> "Greater"
  | Geq -> "Geq"

let rec txt_of_expr = function
  | Int_lit(x) -> "Int_lit(" ^ string_of_int x ^ ")"
  | Float_lit(x) -> "Float_lit(" ^ string_of_float x ^ ")"
  | String_lit(x) -> "String_lit(" ^ x ^ ")"
  | Bool_lit(x) -> "Bool_lit(" ^ string_of_bool x ^ ")"
  | Id(x) -> "Id(" ^ x ^ ")"
  | Unop(op, e) -> "Unop(" ^ (txt_of_op op) ^ ", " ^ (txt_of_expr e) ^ ")"
  | Binop(e1, op, e2) ->
      let v1 = txt_of_expr e1 and op1 = txt_of_op op and v2 = txt_of_expr e2 in
      "Binop(" ^ v1 ^ ", " ^ op1 ^ ", " ^ v2 ^ ")"
  | Call(f, args) ->
      "Call(" ^ (txt_of_expr f) ^ ", [" ^ (txt_of_list args) ^ "])"
  | List(l) -> "List([" ^ (txt_of_list l) ^ "])"

and txt_of_list = function
  | [] -> ""
  | [x] -> txt_of_expr x
  | _ as l -> String.concat " ; " (List.map txt_of_expr l)

let rec txt_of_stmt = function
  | Do(expr) -> let e = txt_of_expr expr in "Do(" ^ e ^ ")"

let rec txt_of_stmts acc = function
  | [] -> "[" ^ (String.concat " ; " acc) ^ "]"
  | stmt :: tl -> txt_of_stmts (txt_of_stmt stmt :: acc) tl

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.program Scanner.token lexbuf in
  let result = txt_of_stmts [] expr in
  print_endline result
