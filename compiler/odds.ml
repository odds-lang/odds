(*
 * COMS4115: Odds abstract syntax tree
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

type action = Compile | Help

let get_help =
  "Odds Usage: odds.sh <flag> <input_file> <output_file>\n" ^
  "  -c\tCompile odds input_file to python code in output_file\n" ^
  "  -h\tDisplay this list of options\n"

let _ =
  let action =
    List.assoc Sys.argv.(1) [("-c", Compile) ; ("-h", Help)] in
  if action = Help then print_endline get_help else
    let lexbuf = Lexing.from_channel stdin in 
    let output_file = Sys.argv.(2) in
    let ast = Parser.program Scanner.token lexbuf in
    let sast = Analyzer.check_ast ast in
    match action with
      | Compile -> Generator.gen_program output_file sast
      | Help -> ()
