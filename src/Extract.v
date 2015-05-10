Require Import FunctionNinjas.All.
Require Import Io.System.All.
Require Io.Exception.
Require Api.
Require Main.

(** The extracted program. *)
Definition opamWebsite : unit :=
  Extraction.launch (fun argv =>
    Main.main argv |>
    Api.Evaluate.eval |>
    Exception.eval |>
    Exception.handle Api.Exc.handle).

Extraction "extraction/opamWebsite" opamWebsite.
