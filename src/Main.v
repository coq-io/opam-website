Require Import Coq.Lists.List.
Require Import Coq.Strings.String.
Require Import FunctionNinjas.All.
Require Import Io.All.
Require Import Io.System.All.
Require Io.List.
Require Import ListString.All.
Require Api.
Require Import Model.
Require View.

Import ListNotations.
Import C.Notations.
Local Open Scope string.
Local Open Scope list.

Definition C := C.t Api.effect.

Definition get_version (name version : LString.t) : C Version.t :=
  let full_name := name ++ LString.s "." ++ version in
  let get_field field := Api.opam_field (LString.s field) full_name in
  let! fields :=
    join (get_field "description") @@
    join (get_field "license") @@
    join (get_field "homepage") @@
    join (get_field "bug-reports") @@
    join (get_field "upstream-url") @@
    get_field "depends" in
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
  end.

Definition get_versions (name : LString.t) : C (list Version.t) :=
  let! versions := Api.opam_versions name in
  Io.List.map_seq (get_version name) versions.

Definition get_packages : C (list Package.t) :=
  let! names := Api.opam_list in
  names |> Io.List.map_seq (fun name =>
    do! Api.log name in
    let! versions := get_versions name in
    ret @@ Package.New name versions).

Definition generate_index (packages : list Package.t) : C unit :=
  Api.write_html (LString.s "index.html") (View.Index.page packages).

Definition generate_version (package : Package.t) (version : Version.t) : C unit :=
  let name := Package.name package ++ LString.s "." ++ Version.version version ++ LString.s ".html" in
  Api.write_html name (View.Version.page package version).

Definition generate_packages (packages : list Package.t) : C unit :=
  packages |> Io.List.iter_seq (fun package =>
    Package.versions package |> Io.List.iter_par (generate_version package)).

Definition main (argv : list LString.t) : C unit :=
  Api.add_debug (
    let! packages := get_packages in
    do! generate_index packages in
    generate_packages packages).
