Require Import Arith.

Inductive arith :=
  | a_plus : arith -> arith -> arith
  | a_var : nat -> arith
  | a_const : nat -> arith
  | a_suc : arith -> arith.

Section t. 
  Variable env : list nat. 
  Fixpoint eval t : nat :=
  match t with
    | a_plus a b => (eval  a) + (eval  b)
    | a_const x => x
    | a_var x => List.nth x env 0
    | a_suc a => S(eval a)
  end.
End t. 


Declare ML Module "plugin".
