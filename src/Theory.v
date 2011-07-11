
(** * Introduction

   The goal of this plugin is to reify concrete terms of type [nat]
   to a language of abstract terms of type [arith]. ([arith] is an
   inductive data-type that represents reified terms of type [nat].)
   This allows to use Barendredgt's so-called 2-level approach.  *)

Require Import Arith.

(** The abstract language of terms of type [nat] (restricted to [plus],
[O], [S] and variables) *)

(* This is not the most simple example of reification possible, since
we could work out an example without variables (with atoms, or holes,
that contains constants). However, using an environment allows one to
reify the identity of such atomic sub-terms. *)

Inductive arith :=
  | a_plus : arith -> arith -> arith
  | a_var : nat -> arith
  | a_const : nat -> arith
  | a_succ : arith -> arith.

(** [eval] maps reified terms of type [arith] to [nat] using an
environment to map syntactic variables to terms. *)

Section t. 
  Variable env : list nat. 
  Fixpoint eval (t : arith) : nat :=
  match t with
    | a_plus a b => (eval  a) + (eval  b)
    | a_const x => x
    | a_var x => List.nth x env 0
    | a_succ a => S (eval a)
  end.
End t. 

(** * Some examples of reified terms  *)
Section example.
  Require Import List. 
  Variables a b c : nat. 
  Let env := a :: b ::c :: nil.

  (** [b + 3] *)
  Eval compute [eval nth env] in 
    (eval env (a_plus (a_var 1) (a_const 3))).

  (** [a + 4] *)
  Eval compute [eval nth env] in 
    (eval env (a_plus (a_var 0) (a_succ (a_const 3)))).

  (** [a * b + 4] *)
  Eval compute [eval nth env] in 
    (eval ((a*b) :: env) (a_plus (a_var 0) (a_succ (a_const 3)))).

  (** [a * b + 4] *)
  Eval compute [eval nth env] in 
    (eval nil (a_plus (a_const (a*b)) (a_succ (a_const 3)))).

End example. 

(** * Some magic  
   
   We use the following vernacular command to make Coq load the plugin
   [plugin] when one load the Coq file [Theory]. In the plugin
   [plugin], we declare some new tactics that will be available in
   proof-mode.

   In the current trunk (07/05/2011 rev 14260), one has to declare all
   ML modules that need to be linked dynamically. *)

Declare ML Module "theplug".
