build:
	ocamlbuild opamWebsite.native -use-ocamlfind -package io-system

clean:
	ocamlbuild -clean

serve:
	ruby -run -e httpd html/ -p 8000
