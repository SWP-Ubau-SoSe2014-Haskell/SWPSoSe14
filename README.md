# A Rail compiler written in Haskell [![Build Status](https://travis-ci.org/SWP-Ubau-SoSe2014-Haskell/SWPSoSe14.svg?branch=master)](https://travis-ci.org/SWP-Ubau-SoSe2014-Haskell/SWPSoSe14)

This is (or rather: will become) a compiler for the esoteric programming
language [Rail](http://esolangs.org/wiki/Rail), written in Haskell.

## Contents of this repository

- `documentation` contains additional documentation.
  - The main documentation will be found in the code.
- `src` contains the Rail compiler and editor (written in Haskell).
- `tests` contains the hunit tests
- `integration-tests` contains integration tests
  - see section `tests` for more information.

## Development

If you plan to contribute to the project,
make sure that your contribution does not break any tests and hlint is happy.

### Coding conventions

Though not applied consistently until now,
there are some things which would be really NICE to have:

- Set indetations to 2 spaces
- Remove trailing white spaces
- Do not retab/reformat other people's code, especially not in a commit which
  contains some logical changes as well
- One logical change per commit
- Integrate [hlint](https://hackage.haskell.org/package/hlint) to your editor of
  choice and try to stick to the suggestions it makes
- Would be cool, if lines are not longer than 80 characters

### Module testing with HUnit

`tests` contains a `Main.hs` file that runs an HUnit test with a list of test
functions. For each module `src/[module-name].hs` of the compiler pipeline
exists a corresponding test file `tests/T[module-name].hs` exporting a list of
test functions for the named module. In the `Main.hs` file the list that is
tested by HUnit, is concatenated by the exported test lists of all test modules.

### Integration tests

Integration tests are stored in `integration-tests` in three subdirectories:
- `passing/` contains tests that are testing already implemented features and
  that already passed before
- `failing/` contains tests that are testing already implemented features but
  never passed
- `future/` contains tests that are testing functionality that will be added
   in the future

Each test consists of two files. A rail program `[test-name].rail` and an
io-file `[test-name].io`.

The io-file specifies test cases, i.e. a set of inputs
with the expected corresponding outputs of the rail-program.

Input and output as well as the test cases themselves are separated by a hash
tag. If an input has more than one value, they are separated by a newline. Consider
a rail program adding two numbers and printing the result (without any newlines). A
corresponding io-file with two test cases could look as follows:

```
3
5
#
8
#
21
56
#
377
```

**NOTE 1:** printed newlines have to be stated explicitly. Consider a hello-world
program printing `Hello World\n` (without any input). The io-file has to look
as follows:

```
#
Hello World\n
```

**NOTE 2:** The expected output is only tested against `stdout`. If you want to test the output
on `stderr` as well, you can add another section to a test case, separated by a single `%` line:

```
This is the input.
#
This is the expected output on stdout.
%
This is the expected output on stderr.
#
Another input.
#
Another stdout output.
```

**NOTE 3:** Lines containing only a single `%` or `#` character always delimit sections as
described above. There is no way to escape them, sorry.

`tests/integration_tests.sh` is a script written in bash. It iterates over all
rail programs in `passing/`, compiles each of them using the current version of
our rail compiler and retrieves runnable llvm-code, i.e. it already links it
with the stack implementation, etc. For each input/output value, it puts the
input into the llvm-binary and compares the actual output with the current
output. The result will be printed to stdout.

(TODO: do we have to run cabal first manually?)

## Dependencies / Building the Compiler

- Install cabal (package cabal-install in most distributions)
- Install llvm, versions llvm-3.3 and llvm-3.4 work.
- run `cabal update`
- If you don't use llvm-3.4 you manually need to install the corresponding haskell bindings, i.e.: `cabal install llvm-general-3.3.11.2`
- Switch to project folder
- Run `cabal install --enable-tests` to install all dependencies and build the project
- `cabal test` to run the tests
- Run the compiler with `dist/build/SWPSoSe14/SWPSoSe14 -c -i <Source.rail> -o output`
- You still need to link the stack manually if you want to have executables:
  `llvm-link <compiled.ll> src/RailCompiler/stack.ll -o executable`

## Documentation

You can generate the compiler documentation using `cabal haddock --executables
--haddock-options --ignore-all-exports` from the root project directory.

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

## Additional Information

For additional information take a look at our wiki pages: https://github.com/SWP-Ubau-SoSe2014-Haskell/SWPSoSe14/wiki
