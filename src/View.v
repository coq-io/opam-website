(** Render a model to HTML. *)
Require Import Coq.Lists.List.
Require Import Coq.NArith.NArith.
Require Import Coq.Strings.Ascii.
Require Import ListPlus.All.
Require Import ListString.All.
Require Import FunctionNinjas.All.
Require Import Model.

Import ListNotations.
Local Open Scope char.

(** The header of the HTML page. *)
Definition header : LString.t :=
  LString.s "<!DOCTYPE html>
<html lang=""en"">
  <head>
    <meta charset=""utf-8"">
    <meta name=""viewport"" content=""width=device-width, initial-scale=1"">
    <title>Coq OPAM</title>
    <link rel=""shortcut icon"" type=""image/png"" href=""img/bag-48.png"" />
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
          <a class=""navbar-brand"" href="".""><img alt=""icon"" src=""img/bag-48.png"" height=""24""> Coq OPAM</a>
        </div>
        <div id=""navbar"" class=""collapse navbar-collapse"">
          <p class=""navbar-text navbar-right""><a class=""navbar-link"" href=""https://github.com/coq/opam-coq-archive"">github.com/coq/opam-coq-archive</a></p>
        </div>
      </div>
      <div class=""row"">
".

(** The footer of the page. *)
Definition footer : LString.t :=
  LString.s
"        </div>
      </div>
      <hr/>
      <div class=""footer"">
        <p class=""text-center"">
          <small>Sources are on <a href=""https://github.com/coq-io/opam-website"">GitHub</a>. Proudly powered by <a href=""http://coq.io/"">Coq.io</a>. © Guillaume Claret</small>
        </p>
      </div>
    </div>
  </body>
</html>
".

(** A package name without the `coq:` prefix. *)
Definition short_name (name : LString.t) : LString.t :=
  match name with
  | _ :: _ :: _ :: _ :: short => short
  | _ => name
  end.

Module Index.
  (** The title with the number of packages. *)
  Definition title (packages : list Package.t) : LString.t :=
    let nb_packages : N := N.of_nat @@ List.length packages in
    let nb_versions : N := N.of_nat @@ Package.number_of_versions packages in
    LString.s "        <div class=""col-md-3"">
          <img alt=""icon"" src=""img/bag.svg"" style=""min-width: 180px; margin-top: 40px"" class=""img-responsive center-block"">
        </div>
        <div class=""col-md-9"">
          <h1>" ++ LString.of_N 10 10 None nb_packages ++
    LString.s " packages <small>" ++ LString.of_N 10 10 None nb_versions ++
    LString.s " versions</small></h1>
          <p><a href=""http://opam.ocaml.org/""> OPAM</a> is the most popular package manager for the <a href=""https://coq.inria.fr/"">Coq</a> community.</p>
          <p>Activate the Coq repository:</p>
          <pre>opam repo add coq-released https://coq.inria.fr/opam/released</pre>
          <p>Read this <a href=""http://coq-blog.clarus.me/make-a-coq-package.html"">tutorial</a> to make your own packages.</p>
        </div>
        <div class=""col-md-12"">
  ".

  (** A row in the table of packages. *)
  Definition row (package : Package.t) : LString.t :=
    let (name, versions) := package in
    match List.rev versions with
    | [] => LString.s ""
    | last_version :: _ =>
      let description := LString.split (Version.description last_version) (LString.Char.n) in
      let description :=
        match description with
        | [] => LString.s ""
        | description :: _ => description
        end in
      LString.s
"              <tr>
                <td class=""text-center""><a href=""./" ++ name ++ LString.s "." ++ Version.version last_version ++ LString.s ".html"">" ++ LString.escape_html (short_name name) ++ LString.s "</a></td>
                <td>" ++ LString.escape_html description ++ LString.s "</td>
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
"        <table class=""table table-striped"">
            <thead>
              <tr>
                <td class=""text-center""><strong>Name</strong></td>
                <td><strong>Description</strong></td>
              </tr>
            </thead>
            <tbody>
" ++ LString.join (LString.s "") (List.map row packages) ++ LString.s
"            </tbody>
          </table>
".

  (** The index page. *)
  Definition page (packages : list Package.t) : LString.t :=
    header ++ title packages ++ table packages ++ footer.
End Index.

Module Version.
  Definition title (name : LString.t) : LString.t :=
    LString.s "        <div class=""col-md-12"">
          <h1>" ++ LString.escape_html (short_name name) ++ LString.s "</h1>
".

  Definition version_link (is_active : bool) (name : LString.t)
    (version : Version.t) : LString.t :=
    let class :=
      if is_active then
        LString.s " class=""active"""
      else
        LString.s "" in
    let url := LString.s "./" ++ name ++ LString.s "." ++ Version.version version ++ LString.s ".html" in
    LString.s "            <li role=""presentation""" ++ class ++
    LString.s "><a href=""" ++ url ++ LString.s """>" ++ LString.escape_html (Version.version version) ++
    LString.s "</a></li>
".

  Definition version_links (name : LString.t) (version : Version.t)
    (versions : list Version.t) : LString.t :=
    LString.s "           <ul class=""nav nav-pills"">
" ++ LString.join (LString.s "") (versions |> List.map (fun version' =>
  version_link (LString.eqb (Version.version version) (Version.version version')) name version')) ++
LString.s "          </ul>
".

  Definition description (name : LString.t) (version : Version.t) : LString.t :=
    LString.s "          <p class=""lead"">" ++ LString.escape_html (Version.description version) ++ LString.s "</p>
          <pre>opam install -j4 " ++ LString.escape_html name ++ LString.s "." ++ LString.escape_html (Version.version version) ++ LString.s "</pre>
".

Definition field (is_url : bool) (name value : LString.t) : LString.t :=
  let value :=
    if is_url then
      LString.s "<a href=""" ++ value ++ LString.s """>" ++ LString.escape_html value ++ LString.s "</a>"
    else
      LString.escape_html value in
  LString.s "            <dt>" ++ LString.escape_html name ++ LString.s "</dt>
            <dd>" ++ value ++ LString.s "</dd>
".

  Definition fields (name : LString.t) (version : Version.t) : LString.t :=
    let full_name := name ++ LString.s "." ++ Version.version version in
    let meta :=
      LString.s "https://github.com/coq/opam-coq-archive/tree/master/released/packages/" ++
      name ++ LString.s "/" ++ full_name in
    LString.s "          <dl class=""dl-horizontal"">
" ++ field true (LString.s "homepage") (Version.homepage version) ++
field false (LString.s "license") (Version.license version) ++
field true (LString.s "bugs tracker") (Version.bug version) ++
field false (LString.s "dependencies") (Version.dependencies version) ++
field true (LString.s "source") (Version.url version) ++
field true (LString.s "package") meta ++
LString.s "          </dl>
".

  Definition page (package : Package.t) (version : Version.t) : LString.t :=
    let (name, versions) := package in
    header ++ title name ++ version_links name version versions ++
    description name version ++ fields name version ++ footer.
End Version.
