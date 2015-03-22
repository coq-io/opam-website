Require Import FunctionNinjas.All.
Require Import Io.System.All.
Require Io.Exception.
Require Api.
Require Main.

(** The extracted program. *)
Definition opamWebsite : unit :=
  Extraction.run (fun argv =>
    Main.main argv |>
    Api.Run.run |>
    Exception.run |>
    Exception.handle Api.Exc.handle).

Extraction "extraction/opamWebsite" opamWebsite.
