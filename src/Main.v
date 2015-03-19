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

Module Version.
  Definition t := LString.t.
End Version.

Module Package.
  Record t := New {
    name : LString.t;
    versions : list Version.t }.
End Package.

Fixpoint print_versions (versions : list Version.t) : C unit :=
  match versions with
  | [] => ret tt
  | version :: versions =>
    do! System.log version in
    print_versions versions
  end.

Definition print_package (package : Package.t) : C unit :=
  do! System.log @@ Package.name package in
  print_versions @@ Package.versions package.

Fixpoint print_packages (packages : list Package.t) : C unit :=
  match packages with
  | [] => ret tt
  | package :: packages =>
    do! print_package package in
    print_packages packages
  end.

Definition get_versions (name : LString.t) : C (list Version.t) :=
  let command := List.map LString.s [
    "opam"; "info"; "--field=available-versions"; LString.to_string name] in
  let! result := System.eval command in
  do! System.log name in
  match result with
  | None =>
    do! System.log @@ LString.s "Cannot run the command to get the list of versions." in
    ret nil
  | Some (status, versions, err) =>
    let! _ : bool := System.print err in
    match status with
    | 0%Z =>
      let versions := LString.split versions (LString.Char.n) in
      let versions := List.map LString.trim versions in
      ret versions
    | _ => ret nil
    end
  end.

Fixpoint get_packages_of_names (names : list LString.t) : C (list Package.t) :=
  match names with
  | [] => ret []
  | name :: names =>
    let! versions := get_versions name in
    let package := Package.New name versions in
    let! packages := get_packages_of_names names in
    ret (package :: packages)
  end.

Definition get_packages : C (list Package.t) :=
  let command := List.map LString.s ["opam"; "search"; "--short"; "coq:"] in
  let! result := System.eval command in
  match result with
  | None =>
    do! System.log @@ LString.s "Cannot run the command the list of packages." in
    ret nil
  | Some (status, names, err) =>
    let! _ : bool := System.print err in
    match status with
    | 0%Z =>
      let names := LString.split names (LString.Char.n) in
      let names := names |> List.filter (fun name =>
        negb @@ LString.is_empty name) in
      get_packages_of_names names
    | _ => ret nil
    end
  end.

Definition main (argv : list LString.t) : C unit :=
  let! packages := get_packages in
  print_packages packages.

(** The extracted program. *)
Definition opamWebsite : unit := Extraction.run main.
Extraction "extraction/opamWebsite" opamWebsite.
