Require Import Coq.Lists.List.
Require Import Coq.Strings.String.
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
  (* let command := List.map LString.s ["opam"; "search"; "coq:"; "--short"] in *)
  (* let command := List.map LString.s ["echo"; "hello world"] in *)
  (* let command := List.map LString.s ["echo"; "hello"; "world"] in *)
  (* let command := List.map LString.s ["ruby"; "-e"; "exit 12"] in *)
  (* let command := List.map LString.s ["ruby"; "-e"; "exit(-12)"] in *)
  let command := List.map LString.s ["ruby"; "-e"; "izaoef"] in
  let! result := System.eval command in
  match result with
  | None =>
    do! System.log @@ LString.s "Cannot run the command" in
    ret nil
  | Some (status, names, err) =>
    let! _ : bool := System.print names in
    let! _ : bool := System.print err in
    do! System.log (LString.of_Z 10 10 status) in
    ret nil
  end.

Definition main (argv : list LString.t) : C unit :=
  let! names := package_names in
  ret tt.

(** The extracted program. *)
Definition opamWebsite : unit := Extraction.run main.
Extraction "extraction/opamWebsite" opamWebsite.
