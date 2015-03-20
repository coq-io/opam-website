Require Import ListString.All.
Require Import Io.All.

Import C.Notations.

Inductive command :=
| Log (message : LString.t)
| OpamList
| OpamField (field package : LString.t)
| WriteHtml (name content : LString.t).

Definition answer (c : command) : Type :=
  match c with
  | Log _ => unit
  | OpamList => LString.t
  | OpamField _ _ => LString.t
  | WriteHtml _ _ => unit
  end.

Definition effect : Effect.t :=
  Effect.New command answer.

Definition log (message : LString.t) : C.t effect unit :=
  call effect (Log message).

Definition opam_list : C.t effect (LString.t) :=
  call effect OpamList.

Definition opam_field (field package : LString.t) : C.t effect LString.t :=
  call effect (OpamField field package).

Definition write_html (name content : LString.t) : C.t effect unit :=
  call effect (WriteHtml name content).
