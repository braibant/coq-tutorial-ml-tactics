(** We reify the structure of coq expressions as an ocaml
    data-type. We reify only the structure of the expression
    w.r.t. the [plus], [S], and [O] symbols of Coq. All other
    sub-expressions are stored in an environment.
*)
module Arith = struct

  (** First, we initialise some constants from Coq standard library.*)
  let plus = lazy (Lib_coq.init_constant ["Coq"; "Init"; "Peano"] "plus")
  let succ = lazy (Lib_coq.init_constant ["Coq"; "Init"; "Datatypes"] "S")
  let zero = lazy (Lib_coq.init_constant ["Coq"; "Init"; "Datatypes"] "O")

  (** [t] is an algebraic data-type that represents reified arithemtic
      expressions *)
  type t =
    | Plus of (t * t)
    | Const of int 		      
    | Succ of t 
    | Var of int 		       

	
  let quote (env : Lib_coq.Env.t) (c : Term.constr) : t =
    (** First, we force the constants, once and for all  *)
    let plus = Lazy.force plus in 
    let succ = Lazy.force succ in 
    let zero = Lazy.force zero in 
    (** Second, we decompose recursively the given term.  If the term
	is an application, we compare the head-symbol with [plus] and
	[succ]. If the term is equal to [zero], we build a
	constant. In any other case, we have to add a new variable to
	the reification environement. *)
    let rec aux c = match Lib_coq.decomp_term c with
      | Term.App (head,args) 
	  when Term.eq_constr head plus && Array.length args = 2
	  -> Plus (aux args.(0), aux args.(1))
      | Term.App (head,args) 
	  when Term.eq_constr head succ && Array.length args =  1 
	  ->
	(** a small match to get a intelligible representation of
	    constants. *)
	begin match (aux args.(0)) with 
	  | Const i -> Const (i +1)
	  | e -> Succ e
	end
      | _ when Term.eq_constr c zero ->
	Const 0
      | _ ->
	let i = Lib_coq.Env.add env c in
	Var i
    in
    aux c
end

(** Now that we have reified the structure of the term inside ocaml,
    we will reify it inside Coq (this is also the purpose of the Quote
    module of standard Coq). 
*)
module Reif = struct
  (** We initialize a new bunch of constants that correspond to the
      constructors of our inductive. *)
    
  (** This [path] correspond to the name of the logical directory
      (ML_tutorial), and the name of the library (Theory). The name of
      the logical directory must be consistent with the options given
      to coq_makefile: [-R ./src ML_tutorial] adds the physical
      directory [src] as the logical directory [ML_tutorial].
  *)
  let path = ["ML_tutorial";"Theory"] 

  let plus = lazy (Lib_coq.init_constant  path "a_plus")
  let var = lazy (Lib_coq.init_constant  path "a_var")
  let const = lazy (Lib_coq.init_constant path  "a_const")
  let succ = lazy (Lib_coq.init_constant path "a_succ")

  (** [eval] is the Coq function that maps a reified Coq arithmetic
      expression back to a nat *)
  let eval = lazy(Lib_coq.init_constant path "eval")

  (** [to_constr t] build the Coq term that corresponds to [t]. *)
  let rec to_constr (t : Arith.t) : Term.constr =  match t with
      | Arith.Plus (a, b) -> Term.mkApp (Lazy.force plus, [|(to_constr a); (to_constr b)|])
      | Arith.Const n -> Term.mkApp (Lazy.force const, [|Lib_coq.Nat.of_int n|])
      | Arith.Succ a -> Term.mkApp (Lazy.force succ, [|(to_constr a)|])
      | Arith.Var n -> Term.mkApp (Lazy.force var, [|Lib_coq.Nat.of_int n|])
	
  (** [env_to_constr env] build the Coq list that correspond to the
      environment map. We build a uniform Coq list of nat of type
      [list nat]. More complex situations may be treated in subsequent
      tutorials. *)
  let env_to_constr (env : Lib_coq.Env.t) : Term.constr = 
    let l = Lib_coq.Env.to_list env in 
    Lib_coq.List.of_list (Lazy.force Lib_coq.Nat.typ) l
      
  (** [build_eval env t] builds the Coq term that corresponds to [eval
      env t]. *)
  let build_eval (env : Lib_coq.Env.t) (t : Arith.t) : Term.constr =
    Lib_coq.lapp eval [|env_to_constr env; to_constr t|]
  (* alternatively, 
     Term.mkApp (Lazy.force eval, [|env_to_constr env; to_constr t|]) *)
      
  (** [tac] is the final tactic. Without too much details, a tactic is
      a function that takes the current goal, and produce some (or
      none) new sub-goals, and a partial proof-tree, that must be
      filled by other tactics.
      
  *)
  let tac : Proof_type.tactic =
    fun goal -> 
      (** We use [Tacmach.pf_concl_goal] to get the conclusion of the
	  goal, which is a constr. (see [proofs/tacmach.mli] for other
	  functions that manipulate the current goal.)  *)
      let concl = Tacmach.pf_concl goal in
      
      (** In our particular setting, the conclusion of the goal must
	  be a relation applied to at least two arguments (the
	  left-hand side and the right-hand side) fo the
	  "equation".  *)
      match Lib_coq.decomp_term concl with
	| Term.App(c, args) when Array.length args >= 2 ->
          let n = Array.length args in
       	  let left = args.(n-2) in
       	  let right = args.(n-1) in 
	  (** The actual relation *)
       	  let r = (Term.mkApp (c, Array.sub args 0 (n - 2))) in
	  (** We initialize the environment, to reify the left
	      hand-side and the right-hand side of the equation*)
       	  let arith_env = Lib_coq.Env.empty () in
       	  let left' = Arith.quote arith_env left in
       	  let right' = Arith.quote arith_env right in
	  
	  (** We want to move from 
	      {C left == right}
	      to
	      {C (eval env left') == (eval env right')}
	      
	  *)
       	  let concl' = Term.mkApp (r, [|build_eval arith_env left'; 
					build_eval arith_env right'|])
	  in
	  (** We use a {i tactical} to chain together a list of
	      tactics (as would be done using a semi-column in Coq).
	      (see [tactics/tacticals.mli] for other tacticals.)  *)
       	  Tacticals.tclTHENLIST 
	    [
	      (** Our list of tactics consists in the following single
       		  tactic, that changes the conclusion of the goal to
       		  [concl'] if [concl] and [concl'] are convertible. 
		  (see [tactics/tactis.mli] for other tactics.)  *)
	      Tactics.change_in_concl None concl';
	    ] goal
	| _ -> 
	  (** If the goal was not looking like a relation applied to two
	      arguments, we fail using the tacticals [tclFAIL]. 
	      
	      The documentation of fail is
	      {{:http://coq.inria.fr/refman/Reference-Manual012.html#@tactic183}here}

	      In a nutshell [tclFAIl] has type [int -> Pp.std_ppcmds ->
	      tactic]. The number is the failure level (0 means that
	      an englobing [match goal] may proceed to the next clause
	      and [try] succeeds, while n > 1 means that the current
	      [match goal] or [try] is aborted, and the level is
	      decremented. 

	      The [Pp.std_ppcmds] is a pretty-printer command. 
	      
	      (see lib/pp.mli for more functions)
	  *)
	  Tacticals.tclFAIL 1
	    (Pp.str "The goal does not look like an equation")
	    goal
end	 
  
  
(** The final magic part is to register our custom tactic in
    Coq. [_reflect_] is the name of this tactic extension (I do not know
    what it is used for). [Reif.tac] is our custom
    tactic. [reflect_arith] is the string through which this tactic
    can be invoked inside Coq. 
*)

TACTIC EXTEND _reflect_
| ["reflect_arith"] -> [Reif.tac]
END
    
