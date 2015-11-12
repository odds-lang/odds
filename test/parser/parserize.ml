open Ast 

(* Binary operators *)
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

(* Expressions *)
let rec txt_of_expr = function
  | Int_lit(x) -> "Int_lit(" ^ string_of_int x ^ ")"
  | Float_lit(x) -> "Float_lit(" ^ string_of_float x ^ ")"
  | String_lit(x) -> "String_lit(" ^ x ^ ")"
  | Bool_lit(x) -> "Bool_lit(" ^ string_of_bool x ^ ")"
  | Id(x) -> "Id(" ^ x ^ ")"
  | Unop(op, e) -> let v = txt_of_expr e and op1 = txt_of_op op in
      "Unop(" ^ op1 ^ ", " ^ v ^ ")"
  | Binop(e1, op, e2) ->
      let v1 = txt_of_expr e1 and op1 = txt_of_op op and v2 = txt_of_expr e2 in
      "Binop(" ^ v1 ^ ", " ^ op1 ^ ", " ^ v2 ^ ")"
  | Call(f, args) -> let args1 = List.map txt_of_expr args in
      "Call(" ^ f ^ ", [" ^ String.concat " ; " args1 ^ "])"
  | Assign(x, e) -> "Assign(" ^ x ^ ", " ^ (txt_of_expr e) ^ " )"
  | Fdecl(f)-> txt_of_fdecl f

and txt_of_exprs exprs =
  let rec aux acc = function
    | [] -> "[" ^ (String.concat " ; " (List.rev acc)) ^ "]"
    | expr :: tl -> aux (txt_of_expr expr :: acc) tl
  in aux [] exprs

(* Function declarations *)
and txt_of_fdecl f =
  "Fdecl({ " ^
    "params=" ^ (txt_of_exprs f.params) ^
    " ; body=" ^ (txt_of_stmts f.body) ^ 
    " ; return=" ^ (txt_of_expr f.return) ^
  " })"

(* Statements *)
and txt_of_stmt = function
  | Do(expr) -> let e = txt_of_expr expr in "Do(" ^ e ^ ")"

and txt_of_stmts stmts =
  let rec aux acc = function
      | [] -> "[" ^ (String.concat " ; " acc) ^ "]"
      | stmt :: tl -> aux (txt_of_stmt stmt :: acc) tl
  in aux [] stmts

(* Program entry point *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let result = txt_of_stmts program in
  print_endline result
