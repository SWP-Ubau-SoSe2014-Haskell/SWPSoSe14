# A Rail compiler written in Haskell [![Build Status](https://travis-ci.org/SWP-Ubau-SoSe2014-Haskell/SWPSoSe14.svg?branch=master)](https://travis-ci.org/SWP-Ubau-SoSe2014-Haskell/SWPSoSe14)

This is (or rather: will become) a compiler for the esoteric programming
language [Rail](http://esolangs.org/wiki/Rail), written in Haskell.

## Contents of this repository

- `documentation` contains additional documentation.
  - The main documentation will be found in the code.
- `src` contains the Rail compiler and editor (written in Haskell).
- `tests` contains the hunit tests
- `rail-examples` contains some Rail example programs

## Development

If you plan to contribute to the project,
make sure that your contribution does not break any tests and hlint is happy.

### Coding conventions

Though not applied consistently until now,
there are some things which would be really NICE to have:

- Set indetations to 2 spaces
- Remove trailing white spaces
- Do not retab/reformat other people's code, especially not in a commit which contains some logical changes as well
- One logical change per commit
- Integrate [hlint](https://hackage.haskell.org/package/hlint) to your editor of choice and try to stick to the suggestions it makes
- Would be cool, if lines are not longer than 80 characters

## Dependencies / Building the Compiler

- Install cabal (package cabal-install in most distributions)
- Install llvm, versions llvm-3.3 and llvm-3.4 work.
- run `cabal update`
- If you don't use llvm-3.4 you manually need to install the corresponding haskell bindings, i.e.: `cabal install llvm-general-3.3.11.2`
- Switch to project folder
- Run `cabal install --enable-tests` to install all dependencies and build the project
- `cabal test` to run the tests
- Run the compiler with `dist/build/SWPSoSe14/SWPSoSe14 --compile <Source.rail> output`

## Branching model

Currently, there are several (long-lived) team branches and one main development branch,
`master`. The `master` branch should always contain something that "works" to
some degree, i. e. it should never break.

All team branches are merged into the `master` branch on a regular basis.

### Team branches

The following team branches exist. Except for `master`, all branches not mentioned
here are to be considered (short-lived) feature branches.

- `gui`: Contains everything with a graphical user interface, most notably the debugger
    and the graphical Rail editor.
- `intertarget-code`: Contains code for the backend, for intermediate code generation and
    for code optimization.
- `preproc-lexer`: Contains code for the preprocessor and lexer components.
- `synsem-analysis`: Contains code for the syntactic/semantic analysis.
