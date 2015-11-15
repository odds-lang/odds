(*
 * COMS4115: Odds abstract syntax tree
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

type action = Compile | Help | Raw

let _ =
  let action =
    List.assoc Sys.argv.(1) [("-c", Compile) ; ("-h", Help) ; ("-r", Raw)] in
  let lexbuf = Lexing.from_channel stdin in 
  let output_file = Sys.argv.(2) in
  let program = Parser.program Scanner.token lexbuf in 
  match action with
    | Compile -> Printer.gen_program output_file program
    | Help -> print_endline "TODO: Add help prompt for compiler"
    | Raw -> Printer.gen_raw output_file program
