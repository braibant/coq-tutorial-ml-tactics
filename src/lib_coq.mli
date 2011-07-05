(** Interface with Coq where we define some handlers for Coq's API,
    and we import several definitions from Coq's standard library.

    This general purpose library could be reused by other plugins.
   
    Some salient points:
    - we use Caml's module system to mimic Coq's one, and avoid cluttering
    the namespace;

*)

(** {2 Getting Coq terms from the environment}  *)

val init_constant : string list -> string -> Term.constr

(** {2 General purpose functions} *)
val decomp_term : Term.constr -> (Term.constr , Term.types) Term.kind_of_term
val lapp : Term.constr lazy_t -> Term.constr array -> Term.constr

(** {2  Getting Coq terms from the environment}  *)
module Env:
sig
  type t 
  val add : t -> Term.constr -> int
  val empty : unit -> t
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

