# Sediment

A syntax-directed metalanguage through which one can type-check and debug the semantics of source programs.
To build and run this project:

- Go to https://docs.haskellstack.org/en/stable/README/ and get the appropriate Stack for your OS.

- Go to https://bnfc.digitalgrammars.com/download/ and get the appropriate BNFC installer for your OS.

- With the terminal location at the root of the project, run `./init.sh`.

- To run Sediment from within the project:
- - To parse a specification in a file: run `Sediment-exe <file-path> <latex>`; `<file-path>` is the relative path to your specification; optionally, type in `<latex>` if you want to generate a .tex file from the specification.
- - To run the REPL run `Sediment-exe`. The REPL is a simple, state-less one.

- Running `init.sh` should expose a globally installed executable. However, if that does not work, the standalone executable can also be found in `./.stack-work/dist/<hash>/build/Sediment-exe/Sediment-exe.exe`.