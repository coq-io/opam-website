Require Import Coq.Lists.List.
Require Import Coq.Strings.String.
Require Import Coq.ZArith.ZArith.
Require Import FunctionNinjas.All.
Require Import Io.System.All.
Require Import ListString.All.

Import ListNotations.
Import C.Notations.
Local Open Scope string.

Definition C := C.t System.effect.

Module Package.
  Record t := {
    name : LString.t;
    version : LString.t }.
End Package.

Definition package_names : C (list LString.t) :=
  let command := List.map LString.s ["opam"; "search"; "--short"; "coq:"] in
  let! result := System.eval command in
  match result with
  | None =>
    do! System.log @@ LString.s "Cannot run the command." in
    ret nil
  | Some (0%Z, names, _) =>
    let names := LString.split names (LString.Char.n) in
    ret @@ List.filter (fun name => negb @@ LString.is_empty name) names
  | Some (_, names, err) =>
    let! _ : bool := System.print names in
    let! _ : bool := System.print err in
    ret nil
  end.

Fixpoint print_names (names : list LString.t) : C unit :=
  match names with
  | [] => ret tt
  | name :: names =>
    do! System.log name in
    print_names names
  end.

Definition main (argv : list LString.t) : C unit :=
  let! names := package_names in
  print_names names.

(** The extracted program. *)
Definition opamWebsite : unit := Extraction.run main.
Extraction "extraction/opamWebsite" opamWebsite.
