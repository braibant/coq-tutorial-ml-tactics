Require Import Arith.

Inductive arith :=
  | a_plus : arith -> arith -> arith
  | a_var : nat -> arith
  | a_const : nat -> arith
  | a_suc : arith -> arith.

Fixpoint eval (env : list nat) t : nat :=
  match t with
    | a_plus a b => (eval env a) + (eval env b)
    | a_const x => x
    | a_var x => List.nth x env 0
    | a_suc a => S(eval env a)
  end.

Fixpoint simpl t :=
  match t with
    | a_plus (a_const 0) x => (simpl x)
    | a_plus x (a_const 0) => (simpl x)
    | _ => t
  end.

Lemma simpl_sound env t : eval env (simpl t) = eval env t.
Admitted.

Lemma reflect env t t': eval env (simpl t) = eval env (simpl t') -> eval env t = eval env t'.
Admitted.