(* The contrib name is used to locate errors when loading constrs *)
let contrib_name = "ml_tutorial"

(** Getting constrs (primitive Coq terms) from existing Coq
   libraries. 
   
    - [find_reference] is located in {v interp/coqlib.ml v} and return a global reference to the name "dir.s" (it must be used lazily). 
    
    - [constr_of_global] is located in {v library/libnames.ml v} and turn a
    global reference into a constr.
*)
let find_constant contrib dir s =
  Libnames.constr_of_global (Coqlib.find_reference contrib dir s)

let init_constant dir s = find_constant contrib_name dir s

(** [decomp_term] returns a user view of a constr, as defined in {v
    kernel/term.ml v}. *)
let decomp_term (c : Term.constr)  = 
  Term.kind_of_term (Term.strip_outer_cast c)
    
let lapp c v  = Term.mkApp (Lazy.force c, v)

module Env = struct
  module ConstrHashed = struct
    type t = Term.constr
    let equal = Term.eq_constr
    let hash = Term.hash_constr
  end
  module ConstrHashtbl = Hashtbl.Make (ConstrHashed)

  type t = (int ConstrHashtbl.t * int ref)
      
  let add (env : t) (t : Term.constr ) =
    try ConstrHashtbl.find (fst env) t 
    with
      | Not_found -> 
	let i = !(snd env) in 
	ConstrHashtbl.add (fst env) t i ; incr (snd env); i

  let empty () = (ConstrHashtbl.create 16, ref 0)	

  let to_list (env,_) = 
    ConstrHashtbl.fold (fun constr key acc -> ( constr) :: acc) env []
      
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



