(** Interface with Coq where we define some handlers for Coq's API,
    and we import several definitions from Coq's standard library.

    This general purpose library is a stripped down version of the one
    built for the plugin aac_tactics.
    
    We use Caml's module system to mimic Coq's one, and avoid
    cluttering the namespace
*)

(** {2 Getting Coq terms from the environment}  *)

(** [init_constant path cst] returns the constr corresponding to
    [path.cst]. It must be used lazily. *)
val init_constant : string list -> string -> Term.constr

(** {2 General purpose functions} *)

(** [decomp_term c] returns a user-view of a term (as defined in the
    module kernel/term.mli). *)
val decomp_term : Term.constr -> (Term.constr , Term.types) Term.kind_of_term

(** [lapp c args] build the application of the lazy constr [c] to the
    array of arguments [args]. This is a handy shortcut. *)
val lapp : Term.constr lazy_t -> Term.constr array -> Term.constr

(** {2  Getting Coq terms from the environment}  *)
module Env:
sig
  (** This module defines a very simple environment for Coq terms. It
      associate an index ([int]) to terms. If a term is added twice,
      then the same index is returned. *)
  
  (** the abstract type of the environment  *)
  type t 
  
  (** [add env c] add a new term to the environment. 

      - If the term [c] is not bound in [env] then we associate a fresh
      index to it, and this pair to [env].

      - If the term [c] is already present in [env], we return its
      index.  
  *)
  val add : t -> Term.constr -> int
    
  (** [empty ()] return an empty environment *)
  val empty : unit -> t

  (** [to_list env] builds the list of the terms that were stored in
      the environment. This allows to access them by their position in
      the list (we ensure that the position in the list is the number
      that was returned when we added them). *)
  val to_list : t -> Term.constr list
end

(** {2 Bindings with Coq' Standard Library}  *)

(** Coq lists *)
module List:
sig
  (** [of_list ty l]  *)
  val of_list:Term.constr ->Term.constr list ->Term.constr
   
  (** [type_of_list ty] *)
  val type_of_list:Term.constr ->Term.constr
end

(** Coq unary numbers (peano) *)
module Nat:
sig
  val typ:Term.constr lazy_t
  val of_int: int ->Term.constr
end

