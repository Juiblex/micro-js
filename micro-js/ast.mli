type binop =
  | Badd | Bsub | Bmul | Bdiv (* int -> int -> int *)
  | Blt | Ble | Bgt | Bge | Beq | Bneq (* int -> int -> bool *)
  | Band | Bor (* bool -> bool -> bool *)
  | Bconc (* string -> string -> string *)

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
  | PVobj of (pident * pexpr) list
  | PVabs of pident list * pstmt (* fun(x1, ..., xn) { s } *)

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
  | PEapp of pexpr * pexpr list (* o.f(x1, ..., xn) *)
  | PEbinop of binop * pexpr * pexpr

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

type pprogram = {prog: pstmt}

(* a store maps variables to heap locations *)
type store = Location.t Map.Make(String).t

type ident = string

type mvalue =
  | MVconst of const
  | MVobj of Location.t (* the location of the object in the object heap *)
  | MVclos of store * (pident list) * pstmt (* closure vars * args * body *)

type mobject = store (* a memory object is really the store of its fields *)

type memory =
  {local: store;
   closure: store;
   global: store}
