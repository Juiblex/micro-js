type binop =
  | Badd | Bsub | Bmul | Bdiv (* int -> int -> int *)
  | Blt | Ble | Bgt | Bge | Beq | Bne (* int -> int -> bool *)
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

type pvalue =
  | PVint of int
  | PVbool of bool
  | PVstring of string
  | PVunit
  | PVobj of (pident * pexpr) list
  | PVabs of pident list * pstmt

and pderef =
  | PDident of pident
  | PDaccess of pexpr * pident

and pexpr = {
  pdesc: pedesc;
  pos: position;
}

and pedesc =
  | PEval of pvalue
  | PEderef of pderef
  | PEapp of pderef * pexpr list (* o.f(x1, ..., xn) *)
  | PEbinop of binop * pexpr * pexpr

and pstmt = {
  pdesc: psdesc;
  pos: position;
}

and psdesc =
  | PSexpr of pexpr
  | PSassign of pderef * pexpr
  | PScond of pexpr * pstmt * pstmt
  | PSloop of pexpr * pstmt
  | PSreturn of pexpr
  | PSblock of pstmt list

