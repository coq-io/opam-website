Require Import Io.All.

Module Command.
  Inductive t (effect : Effect.t) (E : Type) :=
  | Ok (command : Effect.command effect)
  | Error (err : E).
End Command.

Definition answer {effect : Effect.t} {E : Type} (c : Command.t effect E)
  : Type :=
  match c with
  | Command.Ok c => Effect.answer effect c
  | Command.Error err => Empty_set
  end.

Definition effect (effect : Effect.t) (E : Type) : Effect.t :=
  Effect.New (Command.t effect E) answer.
