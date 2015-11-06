(*
 * COMS4115: Odds abstract syntax tree
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

type action = Ast | Gen

let _ =
  let action =
    if Array.length Sys.argv > 1 then 
      List.assoc Sys.argv.(1) [("-a", Ast); ("-g", Gen);]
    else Gen in 
    let lexbuf = Lexing.from_channel stdin in 
    let output_file = String.sub Sys.argv.(2) 0 (String.length(Sys.argv.(2)) - 3) in
    let program = Parser.program Scanner.token lexbuf in 
    match action with
    | Ast ->
        print_string("Todo, add pretty printer for AST")
    | Gen ->
        Generator.gen_program output_file program

