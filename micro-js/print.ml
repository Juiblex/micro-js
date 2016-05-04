open Ast

let bool_to_str = function
  | true -> "true"
  | false -> "false"

let bin_to_str = function
  | Badd -> "Badd"
  | Bsub -> "Bsub"
  | Bmul -> "Bmul"
  | Bdiv -> "Bdiv"
  | Blt -> "Blt"
  | Ble -> "Ble"
  | Bgt -> "Bgt"
  | Bge -> "Bge"
  | Beq -> "Beq"
  | Bneq -> "Bneq"
  | Band -> "Band"
  | Bor -> "Bor"
  | Bconc -> "Bconc"

let hyphens p =
  for i = 0 to 4*p - 1 do
    print_char '-'
  done;
  if p <> 0 then print_string " "

let p_pos p =
  if p.slin = p.elin then
    Printf.printf "line %d, characters %d-%d\n" p.slin p.scol p.ecol
  else
    Printf.printf "line %d:%d - line %d:%d\n" p.slin p.scol p.elin p.ecol

let pprint {pstmts = stmts} =
  let prof = ref 0 in
  let rec p_const c = 
    begin match c with
      | Cint i -> Printf.printf "Cint %d" i
      | Cbool b -> Printf.printf "Cbool %s" (bool_to_str b)
      | Cstring s -> Printf.printf "Cstring \"%s\"" (String.escaped s)
      | Cunit -> Printf.printf "Cunit"
      | Cthis -> Printf.printf "Cthis"
    end;
    print_newline ();
  and p_value v =
    hyphens !prof;
    begin match v with
      | PVconst c -> print_string "PVconst "; p_const c
      | PVobj l -> print_string "PVobj\n"; incr prof;
          List.iter (fun (i, v) -> hyphens !prof; p_ident i; p_value v) l;
          decr prof;
      | PVabs(ids, s) -> print_string "PVabs "; List.iter p_ident ids; p_stmt s
    end;
  and p_ident {pid = i; pos = p} =
    Printf.printf "%s " i; p_pos p;
  and p_deref d =
    hyphens !prof;
    begin match d with
      | PDident i -> print_string "PDident "; p_ident i
      | PDaccess(e, i) -> print_string "PDaccess\n"; p_expr e;
        hyphens !prof;
        p_ident i;
    end;
  and p_expr {pedesc = e; pos = p} =
    hyphens !prof;
    incr prof;
    begin match e with
      | PEvalue v -> print_string "PEvalue "; p_pos p; p_value v
      | PEderef d -> print_string "PEderef "; p_pos p; p_deref d
      | PEapp(d, args) -> print_string "PEapp "; p_pos p; p_deref d;
          List.iter p_expr args;
      | PEbinop(b, e1, e2) -> Printf.printf "PEbinop: %s " (bin_to_str b);
          p_pos p; p_expr e1; p_expr e2;
    end;
    decr prof;
  and p_stmt {psdesc = s; pos = p} =
    hyphens !prof;
    incr prof;
    begin match s with
      | PSexpr e -> print_string "PSexpr "; p_pos p; p_expr e
      | PSassign(d, e) -> print_string "PSassign "; p_pos p; p_deref d; p_expr e
      | PScond(e, s1, s2) -> print_string "PScond "; p_pos p; p_expr e;
          p_stmt s1; p_stmt s2
      | PSloop(e, s) -> print_string "PSloop "; p_pos p; p_expr e; p_stmt s
      | PSreturn e -> print_string "PSreturn "; p_pos p; p_expr e
      | PSblock l -> print_string "PSblock "; p_pos p; List.iter p_stmt l
    end;
    decr prof;
  in List.iter p_stmt stmts;

