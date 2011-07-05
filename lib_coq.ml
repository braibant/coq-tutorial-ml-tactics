(* The contrib name is used to locate errors when loading constrs *)
let contrib_name = "aac_tactics"

(* Getting constrs (primitive Coq terms) from existing Coq
   libraries. *)
let find_constant contrib dir s =
  Libnames.constr_of_global (Coqlib.find_reference contrib dir s)

let init_constant dir s = find_constant contrib_name dir s

(* decomp_term :  constr -> (constr, types) kind_of_term *)
let decomp_term (c : Term.constr) = kind_of_term (strip_outer_cast c)
   
let lapp c v  = mkApp (Lazy.force c, v)

module Env = struct
type t = ((Term.constr, int) Hashtbl.t * int ref)
    
let add (env : t) (t : Term.constr ) =
  try Hashtbl.find (fst env) t 
  with
    | Not_found -> 
      let i = !(snd env) in 
      Hashtbl.add (fst env) t i ; incr (snd env); i

let empty () = (Hashtbl.create 16, ref 0)	

let to_list env = 
  Hashtbl.fold (fun constr key acc -> (key, constr) :: acc) env []

end
   
module Nat = struct
  let path = ["Coq" ; "Init"; "Datatypes"]
  let typ = lazy (init_constant path "nat")
  let _S =      lazy (init_constant  path "S")
  let _O =      lazy (init_constant  path "O")
    (* A coq nat from an int *)
  let of_int n =
    let rec aux n =
      begin  match n with
	| n when n < 0 -> assert false
	| 0 -> Lazy.force _O
	| n -> mkApp
	    (
	      (Lazy.force _S
	      ),  [| aux (n-1)|]
	    )
      end
    in
      aux n
end
   
(** Lists from the standard library*)
module List = struct
  let path = ["Coq"; "Lists"; "List"]
  let typ = lazy (init_constant path "list")
  let nil = lazy (init_constant path "nil")
  let cons = lazy (init_constant path "cons")
  let cons ty h t =
    mkApp (Lazy.force cons, [|  ty; h ; t |])
  let nil ty =
    (mkApp (Lazy.force nil, [|  ty |]))
  let rec of_list ty = function
    | [] -> nil ty
    | t::q -> cons ty t (of_list  ty q)
  let type_of_list ty =
    mkApp (Lazy.force typ, [|ty|])
end

(**[ match_as_equation goal eqt] see [eqt] as an equation. An
   optionnal rel_context can be provided to ensure taht the term
   remains typable*)
let match_as_equation ?(context = Term.empty_rel_context) goal equation : (constr*constr* Std.Relation.t) option  =
  let env = Tacmach.pf_env goal in
  let env =  Environ.push_rel_context context env in
  let evar_map = Tacmach.project goal in
  begin
    match decomp_term equation with
      | App(c,ca) when Array.length ca >= 2 ->
	let n = Array.length ca  in
	let left  =  ca.(n-2) in
	let right =  ca.(n-1) in
	let r = (mkApp (c, Array.sub ca 0 (n - 2))) in
	let carrier =  Typing.type_of env evar_map left in
	let rlt =Std.Relation.make carrier r
	in
	Some (left, right, rlt )
      | _ -> None
  end


include Std
