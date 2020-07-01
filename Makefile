PROJECT=enigma

.PHONY: all
all: build

.PHONY: deps
deps:
	opam install ./${PROJECT}.opam.locked --deps-only

.PHONY: dev
dev:
	opam install . --locked --deps-only

.PHONY: update
update:
	opam install . --deps-only
	make lock

.PHONY: lock
lock:
	opam lock

.PHONY: clean
clean:
	dune clean
	rm */.merlin

.PHONY: build
build:
	dune build @all

.PHONY: utop
utop:
	dune utop .

.PHONY: test
test:
	dune runtest

.PHONY: doc
doc:
	dune build @doc

