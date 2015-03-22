Require Import Io.All.
Require Import ListString.All.
Require Api.
Require Main.
Require Import Model.

Import Run.

Definition get_version (name : LString.t) (version : Version.t)
  : Run.t (Main.get_version name (Version.version version)) version.
  eapply Let.
  - eapply (Join (Api.Spec.opam_field (LString.s "description") _ (Version.description version))).
    eapply (Join (Api.Spec.opam_field (LString.s "license") _ (Version.license version))).
    eapply (Join (Api.Spec.opam_field (LString.s "homepage") _ (Version.homepage version))).
    eapply (Join (Api.Spec.opam_field (LString.s "bug-reports") _ (Version.bug version))).
    eapply (Join (Api.Spec.opam_field (LString.s "upstream-url") _ (Version.url version))).
    apply (Api.Spec.opam_field (LString.s "depends") _ (Version.dependencies version)).
  - destruct version.
    apply Ret.
Defined.
