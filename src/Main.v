Require Import Coq.Lists.List.
Require Import Coq.Strings.String.
Require Import Coq.ZArith.ZArith.
Require Import FunctionNinjas.All.
Require Import Io.System.All.
Require Import ListString.All.
Require Api.
Require Import Model.
Require View.

Import ListNotations.
Import C.Notations.
Local Open Scope string.
Local Open Scope list.

Definition C := C.t Api.effect.

Definition print_version (version : Version.t) : C unit :=
  do! Api.log @@ Version.version version in
  do! Api.log @@ Version.description version in
  do! Api.log @@ Version.license version in
  do! Api.log @@ Version.homepage version in
  do! Api.log @@ Version.bug version in
  do! Api.log @@ Version.url version in
  do! Api.log @@ Version.dependencies version in
  Api.log @@ Version.meta version.

Fixpoint print_versions (versions : list Version.t) : C unit :=
  match versions with
  | [] => ret tt
  | version :: versions =>
    do! print_version version in
    print_versions versions
  end.

Definition print_package (package : Package.t) : C unit :=
  do! Api.log @@ Package.name package in
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
      LString.s "available-versions"
    else
      LString.s "available-version" in
  let! versions := Api.opam_field field name in
  let versions := LString.split versions "," in
  let versions := List.map LString.trim versions in
  let versions := versions |> List.filter (fun version =>
    negb @@ LString.is_empty version) in
  ret versions.

Definition get_version (name version : LString.t) : C Version.t :=
  let full_name := name ++ LString.s "." ++ version in
  let get_field field := Api.opam_field (LString.s field) full_name in
  let! fields :=
    join (get_field "description") @@
    join (get_field "license") @@
    join (get_field "homepage") @@
    join (get_field "bug-reports") @@
    join (get_field "upstream-url") @@
    (get_field "depends") in
  let meta :=
    LString.s "https://github.com/coq/repo-stable/tree/master/packages/" ++
    name ++ LString.s "/" ++ full_name in
  match fields with
  | (description, (license, (homepage, (bug, (url, dependencies))))) =>
    ret @@ Version.New
      version
      description
      license
      homepage
      bug
      url
      dependencies
      meta
  end.

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
    do! Api.log name in
    let! versions := get_versions name in
    let package := Package.New name versions in
    let! packages := get_packages_of_names names in
    ret (package :: packages)
  end.

Definition get_packages : C (list Package.t) :=
  let! names := Api.opam_list in
  let names := LString.split names (LString.Char.n) in
  let names := names |> List.filter (fun name =>
    negb @@ LString.is_empty name) in
  get_packages_of_names names.

Definition generate_index (packages : list Package.t) : C unit :=
  Api.write_html (LString.s "index.html") (View.Index.page packages).

Definition generate_version (package : Package.t) (version : Version.t) : C unit :=
  let name := Package.name package ++ LString.s "." ++ Version.version version ++ LString.s ".html" in
  Api.write_html name (View.Version.page package version).

Fixpoint generate_versions (package : Package.t) (versions : list Version.t)
  : C unit :=
  match versions with
  | [] => ret tt
  | version :: versions =>
    let! _ : unit * unit := join
      (generate_version package version)
      (generate_versions package versions) in
    ret tt
  end.

Fixpoint generate_packages (packages : list Package.t) : C unit :=
  match packages with
  | [] => ret tt
  | package :: packages =>
    do! generate_versions package (Package.versions package) in
    generate_packages packages
  end.

Definition main (argv : list LString.t) : C unit :=
  let! packages := get_packages in
  do! generate_index packages in
  generate_packages packages.

(*
(** The extracted program. *)
Definition opamWebsite : unit := Extraction.run main.
Extraction "extraction/opamWebsite" opamWebsite. *)
