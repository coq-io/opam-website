Require Import Coq.Lists.List.
Require Import FunctionNinjas.All.
Require Import Io.All.
Require IoList.

Import ListNotations.
Import C.Notations.

Module Command.
  Inductive t (E : Effect.t) (Exc : Type) :=
  | Ok (command : Effect.command E)
  | Exc (exc : Exc).
  Arguments Ok [E Exc] _.
  Arguments Exc [E Exc] _.
End Command.

Definition answer {E : Effect.t} {Exc : Type} (c : Command.t E Exc) : Type :=
  match c with
  | Command.Ok c => Effect.answer E c
  | Command.Exc exc => Empty_set
  end.

Definition effect (E : Effect.t) (Exc : Type) : Effect.t :=
  Effect.New (Command.t E Exc) answer.

Definition lift {E : Effect.t} {Exc A : Type} (x : C.t E A)
  : C.t (effect E Exc) A :=
  C.run (fun c => C.Call (effect E Exc) (Command.Ok c)) x.

Definition raise {E : Effect.t} {Exc A : Type} (exc : Exc)
  : C.t (effect E Exc) A :=
  let! absurd := C.Call (effect E Exc) (Command.Exc exc) in
  match absurd with end.

Fixpoint run {E : Effect.t} {Exc A : Type} (x : C.t (effect E Exc) A)
  : C.t E (A + list Exc) :=
  match x with
  | C.Ret _ x => ret @@ inl x
  | C.Call (Command.Ok c) =>
    let! answer := C.Call E c in
    ret @@ inl answer
  | C.Call (Command.Exc exc) => ret @@ inr [exc]
  | C.Let _ _ x f =>
    let! x := run x in
    match x with
    | inl x => run (f x)
    | inr exc => ret @@ inr exc
    end
  | C.Join _ _ x y =>
    let! xy := join (run x) (run y) in
    match xy with
    | (inl x, inl y) => ret @@ inl (x, y)
    | (inr exc, inl _) | (inl _, inr exc) => ret @@ inr exc
    | (inr exc_x, inr exc_y) => ret @@ inr (exc_x ++ exc_y)
    end
  | C.First _ _ x y =>
    let! xy := first (run x) (run y) in
    match xy with
    | inl (inl x) => ret @@ inl @@ inl x
    | inr (inl y) => ret @@ inl @@ inr y
    | inl (inr exc) | inr (inr exc) => ret @@ inr exc
    end
  end.

Definition handle {E : Effect.t} {Exc : Type} (run_exc : Exc -> C.t E unit)
  (x : C.t E (unit + list Exc)) : C.t E unit :=
  let! x := x in
  match x with
  | inl x => ret x
  | inr exc => IoList.iter run_exc exc
  end.
