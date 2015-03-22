Require Import Coq.Lists.List.
Require Import Coq.Strings.String.
Require Import Coq.ZArith.ZArith.
Require Import FunctionNinjas.All.
Require Import ListString.All.
Require Import Io.System.All.
Require Exception.

Import ListNotations.
Import C.Notations.
Local Open Scope string.
Local Open Scope list.

Module Command.
  Inductive t :=
  | Log (message : LString.t)
  | OpamList
  | OpamVersions (package : LString.t)
  | OpamField (field package : LString.t)
  | WriteHtml (name content : LString.t).
End Command.

Definition answer (c : Command.t) : Type :=
  match c with
  | Command.Log _ => unit
  | Command.OpamList => list LString.t
  | Command.OpamVersions _ => list LString.t
  | Command.OpamField _ _ => LString.t
  | Command.WriteHtml _ _ => unit
  end.

Definition effect : Effect.t :=
  Effect.New Command.t answer.

Definition C_api := C.t effect.

Definition log (message : LString.t) : C_api unit :=
  call effect (Command.Log message).

Definition opam_list : C_api (list LString.t) :=
  call effect Command.OpamList.

Definition opam_versions (package : LString.t) : C_api (list LString.t) :=
  call effect (Command.OpamVersions package).

Definition opam_field (field package : LString.t) : C_api LString.t :=
  call effect (Command.OpamField field package).

Definition write_html (name content : LString.t) : C_api unit :=
  call effect (Command.WriteHtml name content).

Module Spec.
  Definition log (message : LString.t) : Run.t (log message) tt.
    apply (Run.Call effect (Command.Log message) tt).
  Defined.

  Definition opam_list (packages : list LString.t) : Run.t opam_list packages.
    apply (Run.Call effect Command.OpamList packages).
  Defined.

  Definition opam_versions (package : LString.t) (versions : list LString.t)
    : Run.t (opam_versions package) versions.
    apply (Run.Call effect (Command.OpamVersions package) versions).
  Defined.

  Definition opam_field (field package value : LString.t)
    : Run.t (opam_field field package) value.
    apply (Run.Call effect (Command.OpamField field package) value).
  Defined.

  Definition write_html (name content : LString.t)
    : Run.t (write_html name content) tt.
    apply (Run.Call effect (Command.WriteHtml name content) tt).
  Defined.
End Spec.

Module Exc.
  Inductive t :=
  | OpamList
  | OpamField (field package : LString.t)
  | WriteHtml (name : LString.t).

  Definition handle (exc : t) : C.t System.effect unit :=
    match exc with
    | OpamList => System.log @@ LString.s "Cannot list the available packages."
    | OpamField field package =>
      System.log (LString.s "Cannot get the field " ++ field ++
        LString.s " of " ++ package ++ LString.s ".")
    | WriteHtml name =>
      System.log (LString.s "Cannot generate the HTML file " ++ name ++
        LString.s ".")
    end.
End Exc.

Definition C_exc := C.t (Exception.effect System.effect Exc.t).

Module Run.
  Definition opam_list : C_exc (list LString.t) :=
    let command := List.map LString.s ["opam"; "search"; "--short"; "coq:"] in
    let! result := Exception.lift @@ System.eval command in
    match result with
    | None => Exception.raise Exc.OpamList
    | Some (status, output, exc) =>
      let! _ : bool := Exception.lift @@ System.print exc in
      match status with
      | 0%Z =>
        let names := LString.split (LString.trim output) (LString.Char.n) in
        ret (names |> List.filter (fun name => negb @@ LString.is_empty name))
      | _ => Exception.raise Exc.OpamList
      end
    end.

  Definition opam_field (field package : LString.t) : C_exc LString.t :=
    let field_command := LString.s "--field=" ++ field in
    let command := [LString.s "opam"; LString.s "info"; field_command; package] in
    let! result := Exception.lift @@ System.eval command in
    match result with
    | None => Exception.raise @@ Exc.OpamField field package
    | Some (status, output, exc) =>
      let! _ : bool := Exception.lift @@ System.print exc in
      match status with
      | 0%Z => ret @@ LString.trim output
      | _ => Exception.raise @@ Exc.OpamField field package
      end
    end.

  Definition opam_versions_aux (is_plural : bool) (package : LString.t)
    : C_exc (list LString.t) :=
    let field :=
      if is_plural then
        LString.s "available-versions"
      else
        LString.s "available-version" in
    let! versions := opam_field field package in
    let versions := LString.split versions "," in
    let versions := List.map LString.trim versions in
    let versions := versions |> List.filter (fun version =>
      negb @@ LString.is_empty version) in
    ret versions.

  Definition opam_versions (package : LString.t) : C_exc (list LString.t) :=
    let! single_version := opam_versions_aux false package in
    let! many_versions := opam_versions_aux true package in
    ret (single_version ++ many_versions).

  Definition write_html (name content : LString.t) : C_exc unit :=
    let file_name := LString.s "html/" ++ name in
    let! is_success := Exception.lift @@ System.write_file file_name content in
    if is_success then
      ret tt
    else
      Exception.raise @@ Exc.WriteHtml name.

  Definition run_command (c : Command.t) : C_exc (answer c) :=
    match c with
    | Command.Log message => Exception.lift @@ System.log message
    | Command.OpamList => opam_list
    | Command.OpamVersions package => opam_versions package
    | Command.OpamField field package => opam_field field package
    | Command.WriteHtml name content => write_html name content
    end.

  Fixpoint run {A : Type} (x : C_api A) : C_exc A :=
    match x with
    | C.Ret _ x => C.Ret x
    | C.Call c => run_command c
    | C.Let _ _ x f => C.Let (run x) (fun x => run (f x))
    | C.Join _ _ x y => C.Join (run x) (run y)
    | C.First _ _ x y => C.First (run x) (run y)
    end.
End Run.
