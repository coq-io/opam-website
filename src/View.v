(** Render a model to HTML. *)
Require Import Coq.Lists.List.
Require Import Coq.NArith.NArith.
Require Import Coq.Strings.Ascii.
Require Import ListPlus.All.
Require Import ListString.All.
Require Import FunctionNinjas.All.
Require Import Model.

Import ListNotations.

(** The header of the HTML page. *)
Definition header : LString.t :=
  LString.s "<!DOCTYPE html>
<html lang=""en"">
  <head>
    <meta charset=""utf-8"">
    <meta name=""viewport"" content=""width=device-width, initial-scale=1"">
    <title>Coq OPAM</title>
    <link rel=""stylesheet"" href=""style.min.css"" type=""text/css"" />
    <!-- HTML5 Shim and Respond.js IE8 support of HTML5 elements and media queries -->
    <!-- WARNING: Respond.js doesn't work if you view the page via file:// -->
    <!--[if lt IE 9]>
      <script src=""https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js""></script>
      <script src=""https://oss.maxcdn.com/respond/1.4.2/respond.min.js""></script>
    <![endif]-->
  </head>
  <body>
    <div class=""container-fluid"">
      <div class=""navbar navbar-default"" role=""navigation"">
        <div class=""navbar-header"">
          <a class=""navbar-brand"" href=""/"">Coq OPAM</a>
        </div>
      </div>
      <div class=""row"">
        <div class=""col-md-12"">
".

(** The footer of the page. *)
Definition footer : LString.t :=
  LString.s
"        </div>
      </div>
      <hr/>
      <div class=""footer"">
        <p class=""text-center"">
          <small>Sources are on <a href=""https://github.com/clarus/coq-opam-website"">GitHub</a>. © Guillaume Claret</small>
        </p>
      </div>
    </div>
  </body>
</html>
".

Module Index.
  (** The title with the number of packages. *)
  Definition title (packages : list Package.t) : LString.t :=
    let nb_packages : N := N.of_nat @@ List.length packages in
    let nb_versions : N := N.of_nat @@ Package.number_of_versions packages in
    LString.s "          <h1>" ++ LString.of_N 10 10 None nb_packages ++
    LString.s " packages <small>" ++ LString.of_N 10 10 None nb_versions ++
    LString.s " versions</small></h1>
        <p>Activate the Coq OPAM repository:</p>
        <pre>opam repo add coq-stable https://github.com/coq/repo-stable.git</pre>
        <p>Install a package:</p>
        <pre>opam install -j4 package</pre>
  ".

  (** A row in the table of packages. *)
  Definition row (package : Package.t) : LString.t :=
    let (name, versions) := package in
    match List.rev versions with
    | [] => LString.s ""
    | last_version :: _ => LString.s
"              <tr>
                <td><a href=""/" ++ name ++ LString.s "." ++ Version.version last_version ++ LString.s ".html"">" ++ name ++ LString.s "</a></td>
                <td>" ++ Version.version last_version ++ LString.s "</td>
                <td>" ++ Version.description last_version ++ LString.s "</td>
              </tr>
"
    end.

  (** The table of packages. *)
  Definition table (packages : list Package.t) : LString.t :=
    let packages := packages |> Sort.sort (fun a b =>
      match LString.compare (Package.name a) (Package.name b) with
      | Lt | Eq => true
      | Gt => false
      end) in
    LString.s
"         <table class=""table table-striped text-center"">
            <thead>
              <tr>
                <td><strong>Name</strong></td>
                <td><strong>Version</strong></td>
                <td><strong>Description</strong></td>
              </tr>
            </thead>
            <tbody>
" ++ LString.join [] (List.map row packages) ++ LString.s
"            </tbody>
          </table>
".

  (** The index page. *)
  Definition page (packages : list Package.t) : LString.t :=
    header ++ title packages ++ table packages ++ footer.
End Index.

Module Version.
  Definition title (name : LString.t) (version : Version.t) : LString.t :=
    LString.s "          <h1>" ++ name ++
    LString.s " <small>" ++ Version.version version ++ LString.s "</small></h1>
".

  Definition description (name : LString.t) (version : Version.t) : LString.t :=
    LString.s "          <p class=""lead"">" ++ Version.description version ++ LString.s "</p>
          <pre>opam install -j4 " ++ name ++ LString.s "." ++ Version.version version ++ LString.s "</pre>
".

Definition field (is_url : bool) (name value : LString.t) : LString.t :=
  let value :=
    if is_url then
      LString.s "<a href=""" ++ value ++ LString.s """>" ++ value ++ LString.s "</a>"
    else
      value in
  LString.s "            <dt>" ++ name ++ LString.s "</dt>
            <dd>" ++ value ++ LString.s "</dd>
".

  Definition fields (version : Version.t) : LString.t :=
    LString.s "          <dl class=""dl-horizontal"">
" ++ field true (LString.s "homepage") (Version.homepage version) ++
field false (LString.s "license") (Version.license version) ++
field true (LString.s "bugs tracker") (Version.bug version) ++
field false (LString.s "dependencies") (Version.dependencies version) ++
field true (LString.s "url") (Version.url version) ++
field true (LString.s "package") (Version.meta version) ++
LString.s "          </dl>
".

  Definition page (name : LString.t) (version : Version.t) : LString.t :=
    header ++ title name version ++ description name version ++ fields version ++ footer.
End Version.
