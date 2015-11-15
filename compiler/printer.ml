(*
 * COMS4115: Post-process printer
 *
 * Authors:
 *  - Alex Kalicki
 *  - Alexandra Medway
 *  - Daniel Echikson
 *  - Lilly Wang
 *)

open Printf

let txt_of_stmts stmts =
  let rec aux acc level = function
    | [] -> String.concat "\n" (List.rev acc)
    | line :: tl -> match line.[0] with
      | '{' -> aux acc (level + 1) tl
      | '}' -> aux acc (level - 1) tl
      | _ -> let s = (String.make level '\t') ^ line in aux (s :: acc) level tl
  in aux [] 0 stmts

(* Code generation entry point *)
let gen_raw output_file program = 
  let text = Generator.txt_of_stmts program in
  let file = open_out output_file in
  fprintf file "%s\n" text; close_out file

let gen_program output_file program =
  let text = Generator.gen_program program in
  let split = Str.split (Str.regexp "\n") in
  let code = txt_of_stmts (split text) in
  let file = open_out output_file in
  fprintf file "%s\n" code; close_out file
