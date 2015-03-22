Require Import Io.All.

Fixpoint run {E1 E2 : Effect.t} {A : Type}
  (run_command : forall (c : Effect.command E1), C.t E2 (Effect.answer E1 c))
  (x : C.t E1 A) : C.t E2 A :=
  match x with
  | C.Ret _ x => C.Ret x
  | C.Call c => run_command c
  | C.Let _ _ x f => C.Let (run run_command x) (fun x => run run_command (f x))
  | C.Join _ _ x y => C.Join (run run_command x) (run run_command y)
  | C.First _ _ x y => C.First (run run_command x) (run run_command y)
  end.
