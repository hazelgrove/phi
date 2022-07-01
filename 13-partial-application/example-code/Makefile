all: fmt build run

fmt:
	dune build @fmt --auto-promote || true

build:
	dune build

run:
	dune exec ./PartialApplication.exe

clean:
	dune clean
