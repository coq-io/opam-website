Require Import Coq.Lists.List.
Require Import Coq.Strings.String.
Require Import FunctionNinjas.All.
Require Import Io.System.All.
Require Import ListString.All.
Require Api.
Require IoList.
Require Import Model.
Require View.

Import ListNotations.
Import C.Notations.
Local Open Scope string.
Local Open Scope list.

Definition C := C.t Api.effect.

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

Definition get_versions (name : LString.t) : C (list Version.t) :=
  let! single_numbers := get_version_numbers false name in
  let! many_numbers := get_version_numbers true name in
  let numbers := single_numbers ++ many_numbers in
  IoList.map (get_version name) numbers.

Definition get_packages : C (list Package.t) :=
  let! names := Api.opam_list in
  let names := LString.split names (LString.Char.n) in
  let names := names |> List.filter (fun name =>
    negb @@ LString.is_empty name) in
  names |> IoList.map (fun name =>
    do! Api.log name in
    let! versions := get_versions name in
    ret @@ Package.New name versions).

Definition generate_index (packages : list Package.t) : C unit :=
  Api.write_html (LString.s "index.html") (View.Index.page packages).

Definition generate_version (package : Package.t) (version : Version.t) : C unit :=
  let name := Package.name package ++ LString.s "." ++ Version.version version ++ LString.s ".html" in
  Api.write_html name (View.Version.page package version).

Definition generate_packages (packages : list Package.t) : C unit :=
  packages |> IoList.iter (fun package =>
    Package.versions package |> IoList.iter_par (generate_version package)).

Definition main (argv : list LString.t) : C unit :=
  let! packages := get_packages in
  do! generate_index packages in
  generate_packages packages.
