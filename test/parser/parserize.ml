open Ast
open Printf

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
  | Int_lit(x) -> sprintf "Int_lit(%s)" (string_of_int x)
  | Float_lit(x) -> sprintf "Float_lit(%s)" (string_of_float x)
  | String_lit(x) -> sprintf "String_lit(%s)" x
  | Bool_lit(x) -> sprintf "Bool_lit(%s)" (string_of_bool x)
  | Id(x) -> sprintf "Id(%s)" x
  | Unop(op, e) -> sprintf "Unop(%s, %s)" (txt_of_op op) (txt_of_expr e)
  | Binop(e1, op, e2) -> sprintf "Binop(%s, %s, %s)"
      (txt_of_expr e1) (txt_of_op op) (txt_of_expr e2)
  | Call(f, args) -> let args1 = List.map txt_of_expr args in
      sprintf "Call(%s, [%s])" (txt_of_expr f) (String.concat " ; " args1)

let rec txt_of_stmt = function
  | Do(e) -> sprintf "Do(%s)" (txt_of_expr e)

let rec txt_of_stmts acc = function
  | [] -> sprintf "[%s]" (String.concat " ; " acc)
  | stmt :: tl -> txt_of_stmts (txt_of_stmt stmt :: acc) tl

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.program Scanner.token lexbuf in
  let result = txt_of_stmts [] expr in
  print_endline result
