# OPAM website
Generation of the Coq website for OPAM. http://coq.io/opam/

## Run
Install the dependencies:

    opam install coq:io:system coq:io:exception

Compile the Coq code:

    ./configure.sh
    make

Compile the extracted OCaml code:

    cd extraction/
    curl -L https://github.com/clarus/coq-red-css/releases/download/coq-blog.1.0.2/style.min.css >html/style.min.css
    make

Run the program:

    cd extraction/
    ./opamWebsite.native
    make serve

You can now browse the result on [localhost:8000](http://localhost:8000/).
