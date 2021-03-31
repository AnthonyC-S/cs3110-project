1. Set your terminal preference to the default style configuration so that all of the colors for the Camlkub tiles show up correctly. 
2. Check that ANSITerminal and OUnit2 (version 2.* or higher) packages are installed and up-to-date.
    - If not, run the following commands on your terminal: “opam install ANSITerminal” and “opam install OUnit2”
    - If your packages are not uptodate, run the following commands on your terminal: “opam update ANSITerminal” and “opam update OUnit2”
3. Check that your OCaml is up-to-date (version 4.11.0 or higher)
4. Run “unzip camlkub.zip” where you want the directory to be located.
5. Run “cd camlkub” to go into the unzipped directory.
6. Run “make build” inside the directory.
7. Run “make play”
