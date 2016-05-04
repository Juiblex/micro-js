{
  open Ast
  open Lexing
  open Parser

  exception Lexing_error of string

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  let kwd_tbl =
    ["if", IF; "then", THEN; "else", ELSE; "while", WHILE; "function", FUNCTION;
     "return", RETURN; "this", THIS; "true", CONST (Ast.Cbool true);
     "false", CONST (Ast.Cbool false)]

  let str_to_char = function
    | "\\\\" -> '\\'
    | "\\\"" -> '"'
    | "\\n" -> '\n'
    | "\\t" -> '\t'
    | s when String.length s = 1 -> s.[0]
    | _ -> raise (Lexing_error "Invalid character constant")

  let string_of_list l =
    String.concat "" (List.map (String.make 1) l)

  let loc startpos endpos =
    { slin = startpos.pos_lnum; scol = startpos.pos_cnum - startpos.pos_bol;
      elin = endpos.pos_lnum; ecol = endpos.pos_cnum - endpos.pos_bol }

  let begin_pos = ref dummy_pos

}

let space = ' ' | '\t'
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let integer = digit+
let ident = letter (letter | '_' | digit)*
let car = [' ' '!' '#'-'[' ']'-'~'] | "\\\\" | "\\\"" | "\\n" | "\tt"

rule token = parse
  | space+ { token lexbuf }
  | '\n' { newline lexbuf; token lexbuf }
  | "//" { comment lexbuf }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '@' { CONC }
  | ">=" { GRE }
  | '>' { GRT }
  | "<=" { LEE }
  | '<' { LET }
  | '=' { ASSIGN }
  | "==" { EQ }
  | "!=" { NEQ }
  | "&&" { AND }
  | "||" { OR }
  | '(' { LP }
  | ')' { RP }
  | '{' { LCB }
  | '}' { RCB }
  | '[' { LSB }
  | ']' { RSB }
  | ',' { COMMA }
  | ';' { SEMICOLON }
  | ':' { COLON }
  | '.' { DOT }
  | '"' { begin_pos := lexbuf.lex_start_p; str_lex [] lexbuf }
  | ident as s {
    if List.exists (fun x -> fst x = s) kwd_tbl then
      List.assoc s kwd_tbl
    else
      IDENT { pid = s; pos = loc lexbuf.lex_start_p lexbuf.lex_curr_p } }
  | integer as s { CONST (Ast.Cint (int_of_string s)) }
  | _ { raise (Lexing_error "Invalid lexem") }
  | eof { EOF }

and comment = parse
  | '\n' { newline lexbuf; token lexbuf }
  | _ { comment lexbuf }
  | eof { EOF }

and str_lex l = parse
  | '"' { CONST (Ast.Cstring(string_of_list (List.rev l))) }
  | car as c { str_lex ((str_to_char c)::l) lexbuf }
  | eof { raise (Lexing_error "Unterminated string constant") }
  | _ { raise (Lexing_error "Invalid string character") }
