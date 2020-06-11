# Sediment

A syntax-directed metalanguage through which one can type-check and debug the semantics of source programs.
To build and run this project:

- Go to https://docs.haskellstack.org/en/stable/README/, get the appropriate Stack for your OS and install it.

- Go to https://bnfc.digitalgrammars.com/download/, get the appropriate BNFC for your OS and install it. BNFC is a language front-end generator, creating several Haskell modules and auxiliary files. These include a module for auto-generated Abstract Syntax, a `.x` file for Lexer generation, and `.y` for Parser generation.

- With the terminal location at the root of the project, run `./init.sh`.
  - The following packages will be installed globally:
    - `alex`, to execute the `.x`.
    - `happy`, to execute the `.y`.
    - `Sediment-exe`, the metalanguage interpreter.

- To run Sediment from within the project:
  - To parse a specification in a file: run `Sediment-exe <file-path> <latex>`; `<file-path>` is the relative path to your specification; optionally, type in `<latex>` if you want to generate a .tex file from the specification. The generated file will be found in `./dist/*` in the current terminal location `Sediment-exe`.
  - To run the REPL run `Sediment-exe`. The REPL is a simple, state-less one.

- Running `init.sh` should expose a globally installed executable. However, if that does not work, the standalone executable can also be found at `./.stack-work/dist/<hash>/build/Sediment-exe/Sediment-exe.exe`.