open Ast

exception Wrong_arity of int * int * position (* expected, actual *)
exception Not_a_function of position
exception Not_an_object of position
exception Not_a_string of position
exception Division_by_zero of position
exception Wrong_type of position
exception Undefined_variable of pident
exception Undefined_field of pident
exception Redefined_field of pident
exception Redefined_argument of pident
exception Assign_primitive of position

exception Return of memory * mvalue (* for the PSreturn statement *)

module Smap = Map.Make(String)

let (vheap : (Location.t, mvalue) Hashtbl.t) = Hashtbl.create 17
let (oheap : (Location.t, mobject) Hashtbl.t) = Hashtbl.create 17

let depth = ref 0 (* function call depth *)
(* We need that to know whether to add new variables to the local or global
   store *)

(* the global object "this" maps to in "function" function calls *)
(* "this" goes into mem.local, so it's a Location.t and not an mvalue! *)
let glob_obj_loc = Location.fresh ()
let glob_obj = MVobj glob_obj_loc (* the location of the store in the oheap *)

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

let rec e_value mem = function
  | PVconst c -> (mem, MVconst c)
  | PVobj fields ->
      let alloc_f (mem, f_locs) ({pid = id} as var, value) =
        if Smap.mem id f_locs then
          raise (Redefined_field var)
        else
        let loc = Location.fresh () in
        let (mem, v) = e_expr mem value in
        Hashtbl.replace vheap loc v;
        (mem, Smap.add id loc f_locs) in
      let (mem, fields) = List.fold_left alloc_f (mem, Smap.empty) fields in
      let loc = Location.fresh () in
      Hashtbl.replace oheap loc fields;
      (mem, MVobj loc)
  | PVabs(args, body) ->
      let clos_vars = Smap.fold Smap.add mem.local mem.closure in
      (mem, MVclos(clos_vars, args, body))

and find_field oloc field = (* go up the prototype chain, return a mvalue *)
  let fields = Hashtbl.find oheap oloc in
  try Hashtbl.find vheap (Smap.find field.pid fields)
  with Not_found ->
    if Smap.mem "__proto__" fields then
      match Hashtbl.find vheap (Smap.find "__proto__" fields) with
        | MVobj oloc -> find_field oloc field
        | _ -> raise (Undefined_field field)
    else raise (Undefined_field field)

and e_deref mem d =
  match d with
    | PDident {pid = "print"} -> (mem, MVconst (Cstring "print"))
    | PDident ({pid = id} as var) -> let loc =
      if Smap.mem id mem.local then
        Smap.find id mem.local
      else if Smap.mem id mem.closure then
        Smap.find id mem.closure
      else if Smap.mem id mem.global then
        Smap.find id mem.global
      else
        raise (Undefined_variable var) in
      (mem, Hashtbl.find vheap loc)
    | PDaccess(o, field) ->
        let (mem, obj) = e_expr mem o in
        begin match obj with
          | MVobj oloc -> (mem, find_field oloc field)
          | _ -> raise (Not_an_object o.pos)
        end
    | PDrefl(o, f) ->
        let (mem, obj) = e_expr mem o in
        match obj with
          | MVobj oloc ->
              let (mem, field) = e_expr mem f in
              begin match field with
                | MVconst (Cstring s) -> (mem, find_field oloc
                                          {pid = s; pos = f.pos})
                | _ -> raise (Not_a_string f.pos)
              end
          | _ -> raise (Not_an_object o.pos)

and e_app mem ({pedesc = f; pos = p} as func) args =
  (* we want to :
    * - get the location l of the caller object in the oheap
    * - create a variable "this" that has a fresh location l'
    * - add Mobj l to the vheap at location l' *)
  let (mem, func, this_loc) = match f with
    | PEderef (PDaccess(o, f)) ->
      let (mem, obj) = e_expr mem o in
      begin match obj with
        | MVobj oloc -> (mem, find_field oloc f, oloc)
        | _ -> raise (Not_an_object o.pos)
      end
    | _ -> let (m, f) = e_expr mem func in (m, f, glob_obj_loc) in

  let rec e_args mem res = function
    | [] -> (mem, List.rev res)
    | a::args -> let (mem', v) = e_expr mem a in
      e_args mem' (v::res) args in

  let (mem, args) = e_args mem [] args in
  match func with
    | MVconst (Cstring "print") ->
        if List.length args <> 1 then
          raise (Wrong_arity(1, List.length args, p))
        else
          print (List.hd args);
          print_newline ();
          (mem, MVconst Cunit)
    | MVclos(clos_vars, params, body) ->
        let alloc_a a_locs ({pid = id} as arg, v) =
          if Smap.mem id a_locs then
            raise (Redefined_argument arg)
          else
          let loc = Location.fresh () in
          Hashtbl.replace vheap loc v;
          Smap.add id loc a_locs in
        let args = try List.combine params args
          with Invalid_argument _ ->
            raise (Wrong_arity(List.length params, List.length args, p)) in
        let local = List.fold_left alloc_a Smap.empty args in
        let loc = Location.fresh () in
        let local = Smap.add "this" loc local in
        Hashtbl.add vheap loc (MVobj this_loc);
        let mem_f = {global = mem.global; closure = clos_vars;
          local = local} in
        begin
          incr depth;
          let (mem_f, res) = try e_stmt mem_f body
            with Return(m, r) -> (m, r) in
          decr depth;
          ({mem with global = mem_f.global}, res)
        end

    | _ -> raise (Not_a_function p)

and e_expr mem {pedesc = e; pos = p} = (* returns (memory, mvalue) *)
  match e with

  | PEvalue v -> e_value mem v

  | PEderef d -> e_deref mem d

  | PEapp(func, args) -> e_app mem func args

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

and e_stmt mem ({psdesc = s; pos = p} as stm) = (* returns (memory, mvalue) *)
  match s with
  | PSexpr e -> e_expr mem e

  | PSassign(var, e) ->
      let assign oloc id = (* for PDaccess and PDrefl *)
        let fields = Hashtbl.find oheap oloc in
        let (mem, v) = e_expr mem e in
        let floc = try Smap.find id fields
          with Not_found -> Location.fresh () in
        begin
          Hashtbl.replace vheap floc v;
          Hashtbl.replace oheap oloc (Smap.add id floc fields);
          (mem, MVconst Cunit)
        end in
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
              | MVobj oloc -> assign oloc id
              | _ -> raise (Not_an_object obj.pos)
            end
        | PDrefl(o, f) ->
            let (mem, obj) = e_expr mem o in
            match obj with
              | MVobj oloc -> let (mem, field) = e_expr mem f in
                  begin match field with
                    | MVconst (Cstring id) -> assign oloc id
                    | _ -> raise (Not_a_string f.pos)
                  end
              | _ -> raise (Not_an_object o.pos)
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

  | PSenum(id, obj, s) ->
      let (mem, o) = e_expr mem obj in
      begin match o with
        | MVobj oloc ->
            (* we don't care whether id was defined before *)
            let loc = Location.fresh () in
            let mem = {mem with local = (Smap.add id.pid loc mem.local)} in
            let eval_step str _ (mem, _) =
              if str <> "__proto__" then
                begin
                  Hashtbl.replace vheap loc (MVconst (Cstring str));
                  e_stmt mem s
                end
              else (mem, MVconst Cunit) in
            Smap.fold eval_step (Hashtbl.find oheap oloc) (mem, MVconst Cunit)
        | _ -> raise (Not_an_object obj.pos)
      end

  | PSdelete d ->
      let remove oloc id =
        begin
          Hashtbl.replace oheap oloc
            (Smap.remove id (Hashtbl.find oheap oloc));
          MVconst (Cbool true)
        end in
      begin match d with
        | PDident i -> (mem, MVconst Cunit) (* does nothing *)
        | PDaccess(obj, {pid = id}) ->
            let (mem, v) = e_expr mem obj in
            begin match v with
              | MVobj oloc -> (mem, remove oloc id)
              | _ -> raise (Not_an_object obj.pos)
            end
        | PDrefl(o, f) ->
            let (mem, obj) = e_expr mem o in
            match obj with
              | MVobj oloc ->
                  let (mem, field) = e_expr mem f in
                  begin match field with
                    | MVconst (Cstring id) -> (mem, remove oloc id)
                    | _ -> raise (Not_a_string f.pos)
                  end
              | _ -> raise (Not_an_object o.pos)
      end

  | PSreturn e ->
      let (mem, v) = e_expr mem e in raise (Return(mem, v))

  | PSblock stmts ->
      let lossy_eval mem s = let (m, v) = e_stmt mem s in m in
      (List.fold_left lossy_eval mem stmts, MVconst Cunit)

and e_prog {prog = s} =
  Hashtbl.add oheap glob_obj_loc Smap.empty;
  e_stmt {local = Smap.empty; closure = Smap.empty; global = Smap.empty} s;
