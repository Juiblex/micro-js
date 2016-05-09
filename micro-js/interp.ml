open Ast

exception Wrong_arity of int * int * position (* expected, actual *)
exception Not_a_function of position
exception Not_an_object of position
exception Division_by_zero of position
exception Wrong_type of position
exception Undeclared_variable of pident
exception Undefined_field of pident
exception Redefined_field of pident
exception Assign_primitive of position

exception Return of memory * mvalue (* for the PSreturn statement *)

module Smap = Map.Make(String)

let (vheap : (Location.t, mvalue) Hashtbl.t) = Hashtbl.create 17
let (oheap : (Location.t, mobject) Hashtbl.t) = Hashtbl.create 17

let depth = ref 0 (* function call depth *)
(* We need that to know whether to add new variables to the local or global 
   store *)

let rec print = function
  | MVconst c -> begin match c with
    | Cint i -> print_int i
    | Cbool b -> print_string (if b then "true" else "false")
    | Cstring s -> print_string s
    | Cunit -> print_string "()"
    end
  | MVobj loc ->
      let rec print_field id l =
        Printf.printf "%s: " id;
        print (Hashtbl.find vheap l);
        print_string "; "  in
      let fields = Hashtbl.find oheap loc in
      print_string "["; Smap.iter print_field fields; print_string "\b\b]"
  | MVclos(_, _, _) -> print_string "function"

(* if we switch object fields to expressions, then that will modify
   the memory *)
let rec e_value mem = function (* doesn't modify the memory, just the heap *)
  | PVconst c -> MVconst c
  | PVobj fields ->
      let alloc_f f_locs ({pid = id} as var, value) =
        if Smap.mem id f_locs then
          raise (Redefined_field var)
        else
        let loc = Location.fresh () in
        let v = e_value mem value in
        Hashtbl.replace vheap loc v;
        Smap.add id loc f_locs in
      let fields = List.fold_left alloc_f Smap.empty fields in
      let loc = Location.fresh () in
      Hashtbl.replace oheap loc fields;
      MVobj loc
  (* the closure variables are the variable that were local at declaration *)
  | PVabs(args, body) ->
      let args = List.map (fun {pid = id} -> id) args in
      MVclos(mem.local, args, body)

and e_expr mem {pedesc = e; pos = p} = (* returns (memory, mvalue) *)
  match e with
  
  | PEvalue v -> (mem, e_value mem v)

  | PEderef d -> begin match d with
    | PDident {pid = "print"} -> (mem, MVconst (Cstring "print"))
    | PDident ({pid = id} as var) -> let loc =
      if Smap.mem id mem.local then
        Smap.find id mem.local
      else if Smap.mem id mem.closure then
        Smap.find id mem.closure
      else if Smap.mem id mem.global then
        Smap.find id mem.global
      else
        raise (Undeclared_variable var) in
      (mem, Hashtbl.find vheap loc)
    | PDaccess(e, ({pid = id} as field)) ->
        let (mem, obj) = e_expr mem e in
        match obj with
          | MVobj oloc -> let fields = Hashtbl.find oheap oloc in
            let floc = try Smap.find id fields
              with Not_found -> raise (Undefined_field field) in
            (mem, Hashtbl.find vheap floc)

          | _ -> raise (Not_an_object p)
  end
  
  | PEapp(func, args) ->
      let (mem, func) = e_expr mem func in
      let rec e_args mem res = function
        | [] -> (mem, List.rev res)
        | a::args -> let (mem', v) = e_expr mem a in
          e_args mem' (v::res) args in
      let (mem, args) = e_args mem [] args in
      begin match func with
        | MVconst (Cstring "print") ->
            if List.length args <> 1 then
              raise (Wrong_arity(1, List.length args, p))
            else
              print (List.hd args);
              print_newline ();
              (mem, MVconst Cunit)
        | MVclos(clos_vars, params, body) ->
            failwith "Closures not implemented yet!"
        | _ -> raise (Not_a_function p)
      end

  | PEbinop(bin, e1, e2) ->
    let (mem, v1) = e_expr mem e1 in
    let (mem, v2) = e_expr mem e2 in
    let res = match bin with
      | Badd | Bsub | Bmul | Bdiv
      | Blt | Ble | Bgt | Bge | Beq | Bneq ->
        begin match (v1, v2) with
          | MVconst (Cint i1), MVconst (Cint i2) ->
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
              | Beq -> Cbool (i1 == i2)
              | Bneq -> Cbool (i1 != i2)
              | _ -> failwith "never happens"
            in MVconst res
          | _ -> raise (Wrong_type p)
        end
      | Band | Bor ->
        begin match (v1, v2) with
          | MVconst (Cbool b1), MVconst (Cbool b2) ->
              let res = match bin with
                | Band -> b1 && b2
                | Bor -> b1 || b2
                | _ -> failwith "never happens"
              in MVconst (Cbool res)
          | _ -> raise (Wrong_type p)
        end
      | Bconc ->
        begin match (v1, v2) with
          | MVconst (Cstring s1), MVconst (Cstring s2) ->
            MVconst (Cstring (String.concat "" [s1; s2]))
          | _ -> raise (Wrong_type p)
        end
    in (mem, res)
  | _ -> failwith "Expression not implemented yet!"

and e_stmt mem ({psdesc = s; pos = p} as stm) = (* returns (memory, mvalue) *)
  match s with
  | PSexpr e -> e_expr mem e

  (* if we're assigning a value that's not an object, copy it; otherwise,
   * copy the location to the object *)
  | PSassign(var, e) ->
      begin match var with
        | PDident {pid = "print"} -> raise (Assign_primitive p)
        | PDident {pid = id} ->
            (* if we're at toplevel, both mem.local and mem.closure are empty *)
            let (mem, v) = e_expr mem e in
            let loc, fresh = 
              if Smap.mem id mem.local then
                Smap.find id mem.local, false
              else if Smap.mem id mem.closure then
                Smap.find id mem.closure, false
              else if Smap.mem id mem.global then
                Smap.find id mem.global, false
              else 
                Location.fresh (), true in
            let mem =
              if fresh then
                if !depth = 0 then (* toplevel *)
                  {mem with global = (Smap.add id loc mem.global)}
                else
                  {mem with local = (Smap.add id loc mem.local)}
              else mem in
            begin
            Hashtbl.replace vheap loc v;
            (mem, MVconst Cunit)
            end
        | PDaccess(obj, {pid = id}) ->
            let (mem, v) = e_expr mem obj in
            begin match v with
              | MVobj oloc ->
                  let fields = Hashtbl.find oheap oloc in
                  let (mem, v) = e_expr mem e in
                  let floc = try Smap.find id fields
                    with Not_found -> Location.fresh () in
                  begin
                    Hashtbl.replace vheap floc v;
                    Hashtbl.replace oheap oloc (Smap.add id floc fields);
                    (mem, MVconst Cunit)
                  end
              | _ -> raise (Not_an_object obj.pos)
            end
      end

  | PScond(cond, s1, s2) ->
      let (mem, v) = e_expr mem cond in
      begin match v with
        | MVconst (Cbool true) -> e_stmt mem s1
        | MVconst (Cbool false) -> e_stmt mem s2
        | _ -> raise (Wrong_type p) 
      end

  | PSloop(cond, s) ->
      let (mem, v) = e_expr mem cond in
      begin match v with
        | MVconst (Cbool true) -> let (mem, _) = e_stmt mem s in e_stmt mem stm
        | MVconst (Cbool false) -> (mem, MVconst Cunit)
        | _ -> raise (Wrong_type p) 
      end

  | PSblock stmts ->
      let lossy_eval mem s = let (m, v) = e_stmt mem s in m in
      (List.fold_left lossy_eval mem stmts, MVconst Cunit)
  | _ -> failwith "Statement not implemented yet!"

and e_prog {prog = s} =
  e_stmt {local = Smap.empty; closure = Smap.empty; global = Smap.empty} s;
