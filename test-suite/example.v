Require Import Theory.

Goal forall a b, a + b + 1 = 0.
  intros. reflect_arith.
Abort.