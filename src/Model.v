Require Import ListString.All.

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

  Definition number_of_versions (packages : list Package.t) : nat :=
    List.fold_left (fun n package => List.length (Package.versions package) + n)
      packages 0.
End Package.
