Require Import Io.System.All.
Require Api.
Require Main.

(** The extracted program. *)
Definition opamWebsite : unit :=
  Extraction.run (fun argv =>
    Api.handle_errors (Api.run (Main.main argv))).

Extraction "extraction/opamWebsite" opamWebsite.
