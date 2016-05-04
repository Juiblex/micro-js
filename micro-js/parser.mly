%{
  open Ast
  open Lexing

  let loc startpos endpos =
    { slin = startpos.pos_lnum; scol = startpos.pos_cnum - startpos.pos_bol;
      elin = endpos.pos_lnum; ecol = endpos.pos_cnum - endpos.pos_bol }

%}

%token EOF
%token <Ast.const> CONST
%token <Ast.pident> IDENT
%token PLUS MINUS TIMES DIV
%token CONC
%token GRE GRT LEE LET EQ NEQ AND OR
%token LP RP LCB RCB LSB RSB COMMA SEMICOLON COLON
%token ASSIGN DOT THIS
%token IF THEN ELSE
%token WHILE
%token FUNCTION RETURN

%left OR
%left AND
%left GRE GRT LEE LET EQ NEQ
%left CONC
%left PLUS MINUS
%left TIMES DIV
%left DOT

%start prog

%type <Ast.pprogram> prog

%%

prog:
  | stmts = cstmt+ EOF
    { {prog = {psdesc = PSblock stmts; pos = loc $startpos $endpos}} }
;

cstmt: (* doing away with lists and whatnot *)
  | s = stmt SEMICOLON { s }
;

block:
  | LCB stmts = cstmt* RCB
    { {psdesc = PSblock stmts; pos = loc $startpos $endpos} }
;

stmt:
  | e = expr { {psdesc = PSexpr e; pos = loc $startpos $endpos} }

  | d = deref ASSIGN e = expr
    { {psdesc = PSassign(d, e); pos = loc $startpos $endpos} }

  | IF LP e = expr RP THEN b1 = block ELSE b2 = block
    { {psdesc = PScond(e, b1, b2); pos = loc $startpos $endpos} }

  | WHILE LP e = expr RP b = block
    { {psdesc = PSloop(e, b); pos = loc $startpos $endpos} }

  | b = block { b }

  | RETURN e = expr? {
    let e = match e with
      | Some pexp -> pexp
      | None -> {pedesc = PEvalue (PVconst Cunit); pos = loc $endpos $endpos} in
    {psdesc = PSreturn e; pos = loc $startpos $endpos} }
;

fdecl:
  | i = IDENT COLON v = value { (i, v) }
;

value:
  | c = CONST { PVconst c }
  | LSB fields = separated_list(COMMA, fdecl) RSB { PVobj fields }
  | FUNCTION LP args = separated_list (COMMA, IDENT) RP body = block
    { PVabs(args, body) }
;

deref:
  | i = IDENT { PDident i }
  | e = expr DOT i = IDENT { PDaccess(e, i) }
;

expr:
  | LP e = expr RP { e }

  | v = value { {pedesc = PEvalue v; pos = loc $startpos $endpos} }

  | d = deref { {pedesc = PEderef d; pos = loc $startpos $endpos} }

  | d = deref LP args = separated_list(COMMA, expr) RP
    { {pedesc = PEapp(d, args); pos = loc $startpos $endpos} }

  | e1 = expr; bin = binop; e2 = expr
    { {pedesc = PEbinop(bin, e1, e2); pos = loc $startpos $endpos} }

  | THIS { {pedesc = PEthis; pos = loc $startpos $endpos} }
;

%inline binop:
  | PLUS { Badd }
  | MINUS { Bsub }
  | TIMES { Bmul }
  | DIV { Bdiv }
  | LET { Blt }
  | LEE { Ble }
  | GRT { Bgt }
  | GRE { Bge }
  | EQ { Beq }
  | NEQ { Bneq }
  | AND { Band }
  | OR { Bor }
  | CONC { Bconc }
