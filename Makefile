all:
	@dune build @all

clean:
	@dune clean

watch:
	@dune build @all -w

onix-lock:
	onix lock ./imandra-document.opam --resolutions="ocaml-system=5.2.0" --lock-file ./onix-lock.json
	onix lock ./imandra-document.opam --resolutions="ocaml-system=5.2.0,ocaml-lsp-server" --with-test=true --lock-file ./onix-lock-dev.json 
	git add onix-lock.json onix-lock-dev.json

