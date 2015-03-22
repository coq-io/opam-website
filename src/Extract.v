Require Import FunctionNinjas.All.
Require Import Io.System.All.
Require Api.
Require Exception.
Require Main.

(** The extracted program. *)
Definition opamWebsite : unit :=
  Extraction.run (fun argv =>
    Main.main argv |>
    Api.run |>
    Exception.run |>
    Exception.handle Api.Exc.handle).

Extraction "extraction/opamWebsite" opamWebsite.
