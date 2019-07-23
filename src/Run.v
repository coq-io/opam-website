Require Import Coq.Lists.List.
Require Import Coq.Strings.String.
Require Import Io.All.
Require Io.List.
Require Import ListString.All.
Require Api.
Require Main.
Require Import Model.

Import Io.Run.

Definition quote (field_content : LString.t) : LString.t :=
  LString.s """" ++ field_content ++ LString.s """".

Lemma unquote_quote (field_content : LString.t) : Main.unquote (quote field_content) = field_content.
  unfold Main.unquote, quote.
  simpl.
  rewrite List.removelast_app.
  - rewrite List.app_nil_r; reflexivity.
  - congruence.
Qed.

Definition get_version (name : LString.t) (version : Version.t)
  : Run.t (Main.get_version name (Version.version version)) version.
  eapply Let.
  - eapply (Join (Api.Run.opam_field (LString.s "synopsis") _ (Version.description version))).
    eapply (Join (Api.Run.opam_field (LString.s "license:") _ (quote (Version.license version)))).
    eapply (Join (Api.Run.opam_field (LString.s "homepage:") _ (quote (Version.homepage version)))).
    eapply (Join (Api.Run.opam_field (LString.s "bug-reports:") _ (quote (Version.bug version)))).
    eapply (Join (Api.Run.opam_field (LString.s "url.src:") _ (quote (Version.url version)))).
    apply (Api.Run.opam_field (LString.s "depends:") _ (Version.dependencies version)).
  - destruct version.
    simpl.
    repeat rewrite unquote_quote.
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
