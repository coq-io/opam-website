# OPAM website
[![Join the chat at https://gitter.im/clarus/coq-opam-website](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/clarus/coq-opam-website?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Generation of the Coq website for OPAM.

## Run
Install the dependencies:

    opam install -j4 coq:io:system coq:io:exception

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
