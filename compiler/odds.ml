(*
 * COMS4115: Odds abstract syntax tree
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

type action = Ast | Compile | Help

let _ =
  let action =
    List.assoc Sys.argv.(1) [("-a", Ast); ("-c", Compile); ("-h", Help)] in
  let lexbuf = Lexing.from_channel stdin in 
  let output_file = Sys.argv.(2) in
  let program = Parser.program Scanner.token lexbuf in 
  match action with
    | Ast -> print_endline "TODO: Add pretty printer for AST"
    | Compile -> Process.gen_program output_file program
    | Help -> print_endline "TODO: Add help prompt for compiler"
