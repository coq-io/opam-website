Require Import Io.All.

Import C.Notations.

Module Command.
  Inductive t (E : Effect.t) (Exc : Type) :=
  | Ok (command : Effect.command E)
  | Exc (exc : Exc).
  Arguments Ok [E Exc] _.
  Arguments Exc [E Exc] _.
End Command.

Definition answer {E : Effect.t} {Exc : Type} (c : Command.t E Exc)
  : Type :=
  match c with
  | Command.Ok c => Effect.answer E c
  | Command.Exc exc => Empty_set
  end.

Definition effect (E : Effect.t) (Exc : Type) : Effect.t :=
  Effect.New (Command.t E Exc) answer.

Fixpoint lift {E : Effect.t} {Exc A : Type} (x : C.t E A)
  : C.t (effect E Exc) A :=
  match x with
  | C.Ret _ x => C.Ret x
  | C.Call c => C.Call (effect E Exc) (Command.Ok c)
  | C.Let _ _ x f => C.Let (lift x) (fun x => lift (f x))
  | C.Join _ _ x y => C.Join (lift x) (lift y)
  | C.First _ _ x y => C.First (lift x) (lift y)
  end.

Definition raise {E : Effect.t} {Exc A : Type} (exc : Exc)
  : C.t (effect E Exc) A :=
  let! absurd := C.Call (effect E Exc) (Command.Exc exc) in
  match absurd with end.
