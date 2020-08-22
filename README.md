# Sediment

A semantics-directed metalanguage interpreter through which one can type-check and debug language semantics given as specifications in a natural semantics.
To build and run this project:

- Go to https://docs.haskellstack.org/en/stable/README/, get the appropriate Stack for your OS and install it.

- Go to https://bnfc.digitalgrammars.com/download/, get the appropriate BNFC for your OS and install it. BNFC is a language front-end generator, creating several Haskell modules and auxiliary files. These include a module for auto-generated Abstract Syntax, a `.x` file for Lexer generation, and `.y` for Parser generation.

- Run `./init.sh`, located in the root of the project.
  - The following packages will be installed globally:
    - `alex`, to execute the `.x` file.
    - `happy`, to execute the `.y` file.
    - `Sediment-exe`, the metalanguage interpreter.

- To run Sediment:
  - Run `Sediment-exe <file-path> <latex>`; `<file-path>` is the relative path to your specification; optionally, type in `<latex>` if you want to generate a `.tex` file from the specification. The generated file will be found in `./dist/*`, generated at the location `Sediment-exe` was run from.
  - Run `Sediment-exe` without any arguments for the interactive interpreter.
  - Alternatively, the project can be run from the local installation through `stack exec Sediment-exe`, giving or omitting arguments as above.

- Running `init.sh` should expose a globally installed executable. However, if that does not work, the standalone executable can also be found at `./.stack-work/dist/<hash>/build/Sediment-exe/Sediment-exe.exe`.
