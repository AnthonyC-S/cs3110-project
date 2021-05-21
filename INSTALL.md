# Installation

## Required Software
This installation assumes you have the following installed and updated:
- **OCaml** (version 4.11.0 or higher) - check with `ocaml --version`.
- **OPAM** (version 2.0 or higher) - check with `opam --version`. You can update packages with `opam update` followed by `opam upgrade`.
- **Make** - check with `make --version`.

## Install the OPAM Dependencies
`opam install ANSITerminal bisect_ppx ounit2 utop`

## Set Up the Game
1. Set your terminal display preference to the default configuration so the Camlkub tiles will display correctly. 
2. Download the camlkub.zip file and run `unzip camlkub.zip` in the desired directory.
3. Run `cd camlkub` to go to unzipped directory.
4. Run `make play` to start the game.

## Other Make Features
- `make test` runs OUnit2 tests.
- `make bisect` runs Bisect test coverage.
- `make docs` generates public and private documenations / specifications.
- `make clean` removes compiled files, etc.