Require Import Io.All.
Require Io.List.
Require Import ListString.All.
Require Api.
Require Main.
Require Import Model.

Import Io.Run.

Definition get_version (name : LString.t) (version : Version.t)
  : Run.t (Main.get_version name (Version.version version)) version.
  eapply Let.
  - eapply (Join (Api.Run.opam_field (LString.s "description") _ (Version.description version))).
    eapply (Join (Api.Run.opam_field (LString.s "license") _ (Version.license version))).
    eapply (Join (Api.Run.opam_field (LString.s "homepage") _ (Version.homepage version))).
    eapply (Join (Api.Run.opam_field (LString.s "bug-reports") _ (Version.bug version))).
    eapply (Join (Api.Run.opam_field (LString.s "upstream-url") _ (Version.url version))).
    apply (Api.Run.opam_field (LString.s "depends") _ (Version.dependencies version)).
  - destruct version.
    apply Ret.
Defined.

Definition get_versions (name : LString.t) (versions : list Version.t)
  : Run.t (Main.get_versions name) versions.
  apply (Let (Api.Run.opam_versions name (List.map Version.version versions))).
  apply (Io.List.Run.map_seq_id versions Version.version).
  apply get_version.
Defined.

Definition get_packages (packages : list Package.t)
  : Run.t Main.get_packages packages.
  apply (Let (Api.Run.opam_list (List.map Package.name packages))).
  apply (Io.List.Run.map_seq_id packages Package.name).
  intro package.
  apply (Let (Api.Run.log (Package.name package))).
  apply (Let (get_versions (Package.name package) (Package.versions package))).
  destruct package.
  apply Ret.
Defined.
