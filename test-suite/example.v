Require Import Theory.

(* example 1 *)
Goal forall a b, a + b + 1 = 0.
  intros. reflect_arith.
Abort.

(* example 2 *)
Goal forall a b c, a + b + 1 + c <= 1.
  intros. reflect_arith.
Abort.

(* example 3 *)
Goal forall a b c, a + b + 1 <= c.
  intros. reflect_arith.
Abort.
