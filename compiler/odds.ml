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

let get_help =
  "Odds Usage: odds.sh <flag> <input_file> <output_file>\n" ^
  "  -c\tCompile odds input_file to python code in output_file\n" ^
  "  -h\tDisplay this list of options\n" ^
  "  -r\tCompile odds input_file to raw, pre-python code in output_file"

let _ =
  let action =
    List.assoc Sys.argv.(1) [("-c", Compile) ; ("-h", Help) ; ("-r", Raw)] in
  if action = Help then print_endline get_help else
    let lexbuf = Lexing.from_channel stdin in 
    let output_file = Sys.argv.(2) in
    let program = Parser.program Scanner.token lexbuf in 
    match action with
      | Compile -> Printer.gen_program output_file program
      | Raw -> Printer.gen_raw output_file program
