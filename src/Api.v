Require Import Coq.Lists.List.
Require Import Coq.Strings.String.
Require Import Coq.ZArith.ZArith.
Require Import FunctionNinjas.All.
Require Import ListString.All.
Require Import Io.System.All.

Import ListNotations.
Import C.Notations.
Local Open Scope string.
Local Open Scope list.

Module Command.
  Inductive t :=
  | Log (message : LString.t)
  | OpamList
  | OpamField (field package : LString.t)
  | WriteHtml (name content : LString.t).
End Command.

Definition answer (c : Command.t) : Type :=
  match c with
  | Command.Log _ => unit
  | Command.OpamList => LString.t
  | Command.OpamField _ _ => LString.t
  | Command.WriteHtml _ _ => unit
  end.

Definition effect : Effect.t :=
  Effect.New Command.t answer.

Definition log (message : LString.t) : C.t effect unit :=
  call effect (Command.Log message).

Definition opam_list : C.t effect (LString.t) :=
  call effect Command.OpamList.

Definition opam_field (field package : LString.t) : C.t effect LString.t :=
  call effect (Command.OpamField field package).

Definition write_html (name content : LString.t) : C.t effect unit :=
  call effect (Command.WriteHtml name content).

Module Error.
  Inductive t :=
  | OpamList
  | OpamField (field package : LString.t)
  | WriteHtml (name : LString.t).

  Definition run (err : t) : C.t System.effect unit :=
    match err with
    | OpamList => System.log @@ LString.s "Cannot list the available packages."
    | OpamField field package =>
      System.log (LString.s "Cannot get the field " ++ field ++
        LString.s " of " ++ package ++ LString.s ".")
    | WriteHtml name =>
      System.log (LString.s "Cannot generate the HTML file " ++ name ++
        LString.s ".")
    end.
End Error.

Definition run_opam_list : C.t System.effect (LString.t + Error.t) :=
  let command := List.map LString.s ["opam"; "search"; "--short"; "coq:"] in
  let! result := System.eval command in
  match result with
  | None => ret @@ inr Error.OpamList
  | Some (status, output, err) =>
    let! _ : bool := System.print err in
    match status with
    | 0%Z => ret @@ inl (LString.trim output)
    | _ => ret @@ inr Error.OpamList
    end
  end.

Definition run_opam_field (field package : LString.t)
  : C.t System.effect (LString.t + Error.t) :=
  let field := LString.s "--field=" ++ field in
  let command := [LString.s "opam"; LString.s "info"; field; package] in
  let! result := System.eval command in
  match result with
  | None => ret @@ inr (Error.OpamField field package)
  | Some (status, output, err) =>
    let! _ : bool := System.print err in
    match status with
    | 0%Z => ret @@ inl (LString.trim output)
    | _ => ret @@ inr (Error.OpamField field package)
    end
  end.

Definition run_write_html (name content : LString.t)
  : C.t System.effect (unit + Error.t) :=
  let file_name := LString.s "html/" ++ name in
  let! is_success := System.write_file file_name content in
  if is_success then
    ret @@ inl tt
  else
    ret @@ inr (Error.WriteHtml name).

Definition run_command (c : Command.t) : C.t System.effect (answer c + Error.t) :=
  match c with
  | Command.Log message =>
    do! System.log message in
    ret (inl tt)
  | Command.OpamList => run_opam_list
  | Command.OpamField field package => run_opam_field field package
  | Command.WriteHtml name content => run_write_html name content
  end.

Fixpoint run {A : Type} (x : C.t effect A) : C.t System.effect (A + Error.t) :=
  match x with
  | C.Ret _ x => ret (inl x)
  | C.Call c => run_command c
  | C.Let _ _ x f =>
    let! x := run x in
    match x with
    | inr err => ret (inr err)
    | inl x => run (f x)
    end
  | C.Join _ _ x y =>
    let! xy := join (run x) (run y) in
    match xy with
    | (inl x, inl y) => ret @@ inl (x, y)
    | (inr err, _) | (_, inr err) => ret (inr err)
    end
  | C.First _ _ x y =>
    let! xy := first (run x) (run y) in
    match xy with
    | inl (inl x) => ret @@ inl (inl x)
    | inr (inl y) => ret @@ inl (inr y)
    | inl (inr err) | inr (inr err) => ret @@ inr err
    end
  end.

Definition handle_errors (x : C.t System.effect (unit + Error.t))
  : C.t System.effect unit :=
  let! x := x in
  match x with
  | inl _ => ret tt
  | inr err => Error.run err
  end.
