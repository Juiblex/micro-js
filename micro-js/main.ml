open Ast
open Lexing
open Interp

let print_past = ref false

let ifile = ref ""

let set_file f s = f := s

let options =
  ["--print-ast", Arg.Set print_past,
   "  Print the abstract syntax tree"]

let usage = "microjs [options] <file>.js"

let loc_p p =
  Printf.printf "File \"%s\", line %d, characters %d-%d:\n" !ifile
    p.slin (p.scol+1) (p.ecol+1)

let location pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  Printf.printf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

let () =
  Arg.parse options (set_file ifile) usage;

  if !ifile == "" then begin
    Printf.printf "Error: no input file\n";
    exit 1;
  end;

  if not (Filename.check_suffix !ifile ".js") then begin
    Printf.printf "Filename must end in .js\n";
    Arg.usage options usage;
    exit 1;
  end;

  let f = open_in !ifile in
  let buf = Lexing.from_channel f in
  try
    let p = Parser.prog Lexer.token buf in
    close_in f;
    if !print_past then begin
      Print.pprint p;
      exit 0;
    end;
  with
    | Lexer.Lexing_error c ->
        location (Lexing.lexeme_start_p buf);
        Printf.printf "Lexing error: %s\n" c;
        exit 1
    | Parser.Error ->
        location (Lexing.lexeme_start_p buf);
        Printf.printf "Parsing error\n";
        exit 1

