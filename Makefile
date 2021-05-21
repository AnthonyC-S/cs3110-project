ML_FILES=$(shell find . -type f -name "*.ml" -not -path "./_build/*")
MLI_FILES=$(shell find . -type f -name "*.mli" -not -path "./_build/*")
MLD_FILES=$(shell find . -type f -name "*.mld" -not -path "./_build/*")
MD_FILES=$(shell find . -type f -name "*.md" -not -path "./_build/*")
WAV_FILES=$(shell find . -type f -name "*.wav" -not -path "./_build/*")
DUNE_FILES=$(shell find . -type f -name "dune" -not -path "./_build/*")


ZIPFILES= $(ML_FILES) $(MLI_FILES) $(MLD_FILES) $(MD_FILES) $(WAV_FILES) dune-project camlkub.opam Makefile .ocamlinit
EXEC=./_build/default/bin/main.exe

RED=\033[0;31m
GREEN=\033[0;32m
BLUE=\033[0;34m
YELLOW=\033[1;33m
BOLD=\033[1m
CLEAR=\033[0m

HOURS_WORKED=echo "$$(cat author.ml) in (print_int hours_worked)" | ocaml  -stdin

default: play

build:
	dune build

play: build
	$(EXEC) -l

serve: build
	$(EXEC) 


clean:
	rm main.byte camlkub.zip .merlin || true
	rm -rf coverage _coverage || true
	dune clean