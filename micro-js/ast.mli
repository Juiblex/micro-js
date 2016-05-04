type binop =
  | Badd | Bsub | Bmul | Bdiv (* int -> int -> int *)
  | Blt | Ble | Bgt | Bge | Beq | Bneq (* int -> int -> bool *)
  | Band | Bor (* bool -> bool -> bool *)
  | Bconc (* string -> string -> string *)

(* Parsing abstract syntax *)

type position = {
  slin: int;
  scol: int;
  elin: int;
  ecol: int;
}

type pident = {
  pid: string;
  pos: position;
}

type const =
  | Cint of int
  | Cbool of bool
  | Cstring of string
  | Cunit

type pvalue =
  | PVconst of const
  | PVobj of (pident * pvalue) list
  | PVabs of pident list * pstmt

and pderef =
  | PDident of pident
  | PDaccess of pexpr * pident

and pexpr = {
  pedesc: pedesc;
  pos: position;
}

and pedesc =
  | PEvalue of pvalue
  | PEderef of pderef
  | PEapp of pderef * pexpr list (* o.f(x1, ..., xn) *)
  | PEbinop of binop * pexpr * pexpr
  | PEthis (* this.stuff *)

and pstmt = {
  psdesc: psdesc;
  pos: position;
}

and psdesc =
  | PSexpr of pexpr
  | PSassign of pderef * pexpr
  | PScond of pexpr * pstmt * pstmt
  | PSloop of pexpr * pstmt
  | PSreturn of pexpr
  | PSblock of pstmt list

type pprogram = {pstmts: pstmt list}
