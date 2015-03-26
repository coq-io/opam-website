Require Import FunctionNinjas.All.
Require Import Io.All.

Import C.Notations.

Module Trace.
  Inductive t (A : Type) :=
  | Ret : t A
  | Call : A -> t A
  | Let : t A -> t A -> t A
  | Join : t A -> t A -> t A
  | First : t A + t A -> t A.
  Arguments Call {A} _.
  Arguments Let {A} _ _.
  Arguments Join {A} _ _.
  Arguments First {A} _.
End Trace.

Fixpoint run {E : Effect.t} {A : Type} (x : C.t E A)
  : C.t E (A * Trace.t {c : Effect.command E & Effect.answer E c}) :=
  match x with
  | C.Ret _ x => ret (x, Trace.Ret _)
  | C.Call c =>
    let! a := call E c in
    ret (a, Trace.Call (existT _ c a))
  | C.Let _ _ x f =>
    let! x := run x in
    let (x, trace_x) := x in
    let! y := run (f x) in
    let (y, trace_y) := y in
    ret (y, Trace.Let trace_x trace_y)
  | C.Join _ _ x y =>
    let! xy := join (run x) (run y) in
    match xy with
    | ((x, trace_x), (y, trace_y)) => ret ((x, y), Trace.Join trace_x trace_y)
    end
  | C.First _ _ x y =>
    let! xy := first (run x) (run y) in
    match xy with
    | inl (x, trace_x) => ret (inl x, Trace.First (inl trace_x))
    | inr (y, trace_y) => ret (inr y, Trace.First (inr trace_y))
    end
  end.
