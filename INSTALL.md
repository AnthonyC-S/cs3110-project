# Install Instructions


This installation assumes you have the following installed:
- OCaml installed and is up-to-date (version 4.11.0 or higher).
- An updated'code'opam'code'
- 'code'make'code' is also installed. You can check by running 'code'make --version'code'.


1. Set your terminal preference to the default style configuration so that all of the colors for the Camlkub tiles show up correctly. 
2. Check that ANSITerminal and OUnit2 (version 2.* or higher) packages are installed and up-to-date.
    - If not, run the following commands on your terminal: 'code'opam install ANSITerminal'code' and 'code'opam install OUnit2'code'
    - If your packages are not uptodate, run the following commands on your terminal: 'code'opam update ANSITerminal'code' and 'code'opam update OUnit2'code'
3. Check that your OCaml is up-to-date (version 4.11.0 or higher)
4. Run 'code'unzip camlkub.zip'code' where you want the directory to be located.
5. Run 'code'cd camlkub'code' to go into the unzipped directory.
6. Run 'code'make build'code' inside the directory.
7. Run 'code'make play'code' to start the game.

Other 
