(* The contrib name is used to locate errors when loading constrs *)
let contrib_name = "ml_tutorial"

(* Getting constrs (primitive Coq terms) from existing Coq
   libraries. *)
let find_constant contrib dir s =
  Libnames.constr_of_global (Coqlib.find_reference contrib dir s)

let init_constant dir s = find_constant contrib_name dir s

(* decomp_term :  constr -> (constr, types) kind_of_term *)
let decomp_term (c : Term.constr) = Term.kind_of_term (Term.strip_outer_cast c)
   
let lapp c v  = Term.mkApp (Lazy.force c, v)

module Env = struct
type t = ((Term.constr, int) Hashtbl.t * int ref)
    
let add (env : t) (t : Term.constr ) =
  try Hashtbl.find (fst env) t 
  with
    | Not_found -> 
      let i = !(snd env) in 
      Hashtbl.add (fst env) t i ; incr (snd env); i

let empty () = (Hashtbl.create 16, ref 0)	

let to_list (env,_) = 
  Hashtbl.fold (fun constr key acc -> ( constr) :: acc) env []
    
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
	| n -> Term.mkApp
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
    Term.mkApp (Lazy.force cons, [|  ty; h ; t |])
  let nil ty =
    (Term.mkApp (Lazy.force nil, [|  ty |]))
  let rec of_list ty = function
    | [] -> nil ty
    | t::q -> cons ty t (of_list  ty q)
  let type_of_list ty =
    Term.mkApp (Lazy.force typ, [|ty|])
end



