Require Import Coq.Lists.List.
Require Import Io.All.

Import ListNotations.
Import C.Notations.

Fixpoint map {E : Effect.t} {A B : Type} (f : A -> C.t E B) (l : list A)
  : C.t E (list B) :=
  match l with
  | [] => ret []
  | x :: l =>
    let! y := f x in
    let! l := map f l in
    ret (y :: l)
  end.

Fixpoint iter {E : Effect.t} {A : Type} (f : A -> C.t E unit) (l : list A)
  : C.t E unit :=
  match l with
  | [] => ret tt
  | x :: l =>
    do! f x in
    iter f l
  end.

Fixpoint iter_par {E : Effect.t} {A : Type} (f : A -> C.t E unit) (l : list A)
  : C.t E unit :=
  match l with
  | [] => ret tt
  | x :: l =>
    let! _ : unit * unit := join (f x) (iter_par f l) in
    ret tt
  end.
