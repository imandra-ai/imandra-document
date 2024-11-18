all:
	@dune build @all

clean:
	@dune clean

watch:
	@dune build @all -w

nix-lock:
	nix run .#onix-lock

.PHONY: onix-lock.json
onix-lock.json:
	onix lock ./imandra-document.opam --resolutions="ocaml-system=5.2.0" --lock-file ./onix-lock.json

.PHONY: onix-lock-dev.json
onix-lock-dev.json:
	onix lock ./imandra-document.opam --resolutions="ocaml-system=5.2.0,ocaml-lsp-server,utop" --with-test=true --lock-file ./onix-lock-dev.json

.PHONY: lock
lock: onix-lock.json onix-lock-dev.json
	git add onix-lock.json onix-lock-dev.json

