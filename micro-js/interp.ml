open Ast

exception Wrong_arity of int * int * position (* expected, actual *)
exception Not_a_function of position
exception Division_by_zero of position
exception Wrong_type of position

module Loc = struct
  type t = int
  let compare l1 l2 = Pervasives.compare l1 l2
  let equal l1 l2 = l1 = l2
  let create = let r = ref 0 in fun () -> incr r; !r
end

module Lmap = Map.Make(Loc)

module Smap = Map.Make(String)

type memory = {heap: pvalue Lmap.t; (* the heap : location -> value *)
               local: Loc.t Smap.t; (* memories : ident -> location *)
               clos: Loc.t Smap.t; 
               glob: Loc.t Smap.t}

let rec print = function
  | PVconst c -> begin match c with
    | Cint i -> print_int i
    | Cbool b -> print_string (if b then "true" else "false")
    | Cstring s -> print_string s
    | Cunit -> print_string "()"
    end
  | PVobj fields ->
    let rec print_field = function
      | [] -> ()
      | [(id, v)] -> Printf.printf "%s: " id.pid; print v
      | (id, v)::fs -> Printf.printf "%s: " id.pid; print v; print_string "; ";
        print_field fs in print_field fields
  | PVabs(_, _) -> print_string "function"

let rec get_val mem deref = (* turn a dereference into an actual value *)
  match deref with
    | PDident {pid = "print"} -> (mem, PVconst (Cstring "print"))
    | _ -> failwith "Val not implemented yet!"

let rec e_expr mem {pedesc = e; pos = p} = (* returns (memory, value) *)
  match e with
    | PEvalue v -> (mem, v)

    | PEapp(func, args) ->
      let (mem, func) = get_val mem func in (* func is now a pvalue *)
      let rec e_args mem res = function (* left-to-right evaluation *)
        | [] -> (mem, List.rev res)
        | a::args -> let (mem', v) = e_expr mem a in
          e_args mem' (v::res) args in
      let (mem, args) = e_args mem [] args in
      begin match func with
        | PVconst (Cstring "print") ->
            if (List.length args <> 1) then
              raise (Wrong_arity(1, List.length args, p))
            else
              print (List.hd args);
              (mem, PVconst Cunit)
        | PVabs(params, body) -> failwith "Call not implemented yet!"
        | _ -> raise (Not_a_function p)
      end

    | PEbinop(bin, e1, e2) ->
        let (mem, v1) = e_expr mem e1 in
        let (mem, v2) = e_expr mem e2 in
        let res = match bin with
          | Badd | Bsub | Bmul | Bdiv
          | Blt | Ble | Bgt | Bge | Beq | Bneq ->
            begin match (v1, v2) with
              | PVconst (Cint i1), PVconst (Cint i2) ->
                let res = match bin with
                  | Badd -> Cint (i1 + i2)
                  | Bsub -> Cint (i1 - i2)
                  | Bmul -> Cint (i1 * i2)
                  | Bdiv -> if i2 = 0 then raise (Division_by_zero e2.pos)
                      else Cint (i1 / i2)
                  | Blt -> Cbool (i1 < i2)
                  | Ble -> Cbool (i1 <= i2)
                  | Bgt -> Cbool (i1 > i2)
                  | Bge -> Cbool (i1 >= i2)
                  | _ -> failwith "never happens"
                in PVconst res
              | _ -> raise (Wrong_type p)
            end
          | Band | Bor ->
            begin match (v1, v2) with
              | PVconst (Cbool b1), PVconst (Cbool b2) ->
                  let res = match bin with
                    | Band -> b1 && b2
                    | Bor -> b1 || b2
                    | _ -> failwith "never happens"
                  in PVconst (Cbool res)
              | _ -> raise (Wrong_type p)
            end
          | Bconc ->
            begin match (v1, v2) with
              | PVconst (Cstring s1), PVconst (Cstring s2) ->
                PVconst (Cstring (String.concat "" [s1; s2]))
              | _ -> raise (Wrong_type p)
            end
        in (mem, res)

    | _ -> failwith "Expression not implemented yet!"

and e_stmt mem {psdesc = s; pos = p} = (* returns (memory, value) *)
  match s with
    | PSexpr e -> e_expr mem e
    | PSblock stmts ->
      let lossy_eval mem s = let (m, v) = e_stmt mem s in m in
      (List.fold_left lossy_eval mem stmts, PVconst Cunit)
    | _ -> failwith "Statement not implemented yet!"; 

and e_prog {prog = s} =
  e_stmt {heap = Lmap.empty; local = Smap.empty; clos = Smap.empty;
    glob = Smap.empty} s;
