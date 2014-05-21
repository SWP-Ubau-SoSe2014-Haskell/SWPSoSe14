#!/bin/bash

### Function for reading in-/output files
function readtest {
  FILE=$1
  IFS= read -rd '' content < "$FILE" 
  echo -n "$content"
}


### Directory magic, so our cwd is the directory where the script resides.
unset CDPATH
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
cd "$DIR/.."

TESTDIR="rail-examples"

### Compile and run all .rail files
TMPDIR=tests/tmp
mkdir $TMPDIR
for f in $TESTDIR/hello-world.rail
#for f in rail-examples/*.rail 
do
  filename="${f##*/}"
  filename="${filename%%.*}"
  if [ -f "$TESTDIR/$filename.test" ]
    then
      readtest "$TESTDIR/$filename.test"
  fi
  dist/build/SWPSoSe14/SWPSoSe14 --compile "$f" "$TMPDIR/$filename.ll"
  llvm-link "$TMPDIR/$filename.ll" src/RailCompiler/stack.ll > "$TMPDIR/$filename"
  chmod +x "$TMPDIR/$filename"
  output=$("$TMPDIR/$filename")
done

rm -r tests/tmp
