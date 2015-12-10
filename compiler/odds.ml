(*
 * COMS4115: Odds abstract syntax tree
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

open Printf

type action = Compile | Help | Raw | Sast

let get_help =
  "Odds Usage: odds.sh <flag> <input_file> <output_file>\n" ^
  "  -c\tCompile odds input_file to python code in output_file with stdlib\n" ^
  "  -r\tCompile odds input_file into raw python output_file\n" ^
  "  -s\tCompile odds input_file into semantically checked ast\n" ^
  "  -h\tDisplay this list of options\n"

let _ =
  let action =
    List.assoc Sys.argv.(1) 
      [("-c", Compile) ; ("-h", Help) ; ("-r", Raw); ("-s", Sast)] in
  if action = Help then print_endline get_help else
  try
    let lexbuf = Lexing.from_channel stdin in 
    let ast = Parser.program Scanner.token lexbuf in
    let sast = Analyzer.check_ast ast in
    let past = Pythonizer.generate_past sast in
    let prog = Generator.gen_program past in
    match action with
      | Compile -> 
          let output_file = Sys.argv.(2) in
          let stdlib = Utils.get_str_from_file "stdlib.py" in  
          let file = open_out output_file
          in fprintf file "%s%s\n" stdlib prog; close_out file
      | Raw -> 
          let output_file = Sys.argv.(2) in
          let file = open_out output_file
          in fprintf file "%s\n" prog; close_out file
      | Sast -> Printer.print_sast ast 
      | Help -> ()
  with 
    | Scanner.Illegal_Character(m) -> eprintf "Scanner Exception: %s\n" m
    | Analyzer.Semantic_Error(m) -> eprintf "Analyzer Exception: %s\n" m
