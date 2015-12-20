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
  "Odds Usage: odds.sh <flag> [input_file] [output_file]\n" ^
  "  -c\tCompile odds input_file to python code in output_file with stdlib\n" ^
  "  -h\tDisplay this list of options\n" ^
  "  -r\tCompile odds input_file into raw python output_file\n" ^
  "  -s\tPrint odds input_file as semantically checked ast"

(* Error reporting helper function *)
let get_pos_and_tok lexbuf = 
  let cur = lexbuf.Lexing.lex_curr_p in
  let line_num = cur.Lexing.pos_lnum and
    column_num = cur.Lexing.pos_cnum - cur.Lexing.pos_bol and
    token = Lexing.lexeme lexbuf in
  line_num, column_num, token

let _ =
  let action = List.assoc Sys.argv.(1)
    [("-c", Compile) ; ("-h", Help) ; ("-r", Raw); ("-s", Sast)] in
  if action = Help then print_endline get_help else
  let lexbuf = Lexing.from_channel stdin in
  try 
    let ast = Parser.program Scanner.token lexbuf in
    let sast = Analyzer.check_ast ast in
    let past = Pythonizer.generate_past sast in
    let prog = Generator.gen_program past in
    match action with
      | Compile -> 
          let output_file = Sys.argv.(2) and stdlib_file = Sys.argv.(3) in
          let stdlib = Utils.str_of_file stdlib_file in
          let file = open_out output_file
          in fprintf file "%s\n\n%s\n%s\n"
            stdlib prog (Utils.conclude_program ()); close_out file
      | Raw -> 
          let output_file = Sys.argv.(2) in
          let file = open_out output_file
          in fprintf file "%s\n" prog; close_out file
      | Sast -> Printer.print_sast sast
      | Help -> print_endline get_help
  with 
    | Scanner.Illegal_Character(m) -> 
        let line_num, column_num, _ = get_pos_and_tok lexbuf in
        eprintf 
          "\x1b[31mSyntax error\x1b[0m, line %d at column %d: %s\n" 
          line_num column_num m 
    | Analyzer.Semantic_Error(m) -> 
        let line_num, _, _ = get_pos_and_tok lexbuf in
        eprintf "\x1b[31mSemantic error\x1b[0m, line %d:\n  %s\n" 
          line_num m
    | Parsing.Parse_error -> 
        let line_num, column_num, token = get_pos_and_tok lexbuf in
        eprintf 
        "\x1b[31mSyntax error\x1b[0m, line %d at column %d: '%s'\n"
        line_num column_num token
