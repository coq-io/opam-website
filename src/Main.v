Require Import Coq.Lists.List.
Require Import Coq.Strings.String.
Require Import Coq.ZArith.ZArith.
Require Import FunctionNinjas.All.
Require Import Io.System.All.
Require Import ListString.All.

Import ListNotations.
Import C.Notations.
Local Open Scope string.
Local Open Scope list.

Definition C := C.t System.effect.

Module Version.
  Record t := New {
    version : LString.t;
    description : LString.t;
    license : LString.t;
    homepage : LString.t;
    bug : LString.t;
    url : LString.t;
    dependencies : LString.t;
    meta : LString.t }.
End Version.

Module Package.
  Record t := New {
    name : LString.t;
    versions : list Version.t }.
End Package.

Definition print_version (version : Version.t) : C unit :=
  do! System.log @@ Version.version version in
  do! System.log @@ Version.description version in
  do! System.log @@ Version.license version in
  do! System.log @@ Version.homepage version in
  do! System.log @@ Version.bug version in
  do! System.log @@ Version.url version in
  do! System.log @@ Version.dependencies version in
  System.log @@ Version.meta version.

Fixpoint print_versions (versions : list Version.t) : C unit :=
  match versions with
  | [] => ret tt
  | version :: versions =>
    do! print_version version in
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

Definition get_version_numbers (is_plural : bool) (name : LString.t)
  : C (list LString.t) :=
  let field :=
    if is_plural then
      "--field=available-versions"
    else
      "--field=available-version" in
  let command := List.map LString.s [
    "opam"; "info"; field; LString.to_string name] in
  let! result := System.eval command in
  match result with
  | None =>
    do! System.log @@ LString.s "Cannot run the command to get the list of versions." in
    ret nil
  | Some (status, versions, err) =>
    let! _ : bool := System.print err in
    match status with
    | 0%Z =>
      let versions := LString.split versions "," in
      let versions := List.map LString.trim versions in
      let versions := versions |> List.filter (fun version =>
        negb @@ LString.is_empty version) in
      ret versions
    | _ => ret nil
    end
  end.

Definition get_version (name version : LString.t) : C Version.t :=
  ret @@ Version.New
    version
    []
    []
    []
    []
    []
    []
    [].

Fixpoint get_versions_of_numbers (name : LString.t) (numbers : list LString.t)
  : C (list Version.t) :=
  match numbers with
  | [] => ret []
  | number :: numbers =>
    let! version := get_version name number in
    let! versions := get_versions_of_numbers name numbers in
    ret (version :: versions)
  end.

Definition get_versions (name : LString.t) : C (list Version.t) :=
  let! single_numbers := get_version_numbers false name in
  let! many_numbers := get_version_numbers true name in
  let numbers := single_numbers ++ many_numbers in
  get_versions_of_numbers name numbers.

Fixpoint get_packages_of_names (names : list LString.t) : C (list Package.t) :=
  match names with
  | [] => ret []
  | name :: names =>
    do! System.log name in
    let! package_packages := join
      (let! versions := get_versions name in
      ret @@ Package.New name versions)
      (get_packages_of_names names) in
    let (package, packages) := package_packages in
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
