#!/bin/bash

### Function for reading in-/output files
function readtest {
  FILE=$1
  i=1
  modeIn=true
  IN[1]=""
  OUT[1]=""
  while IFS= read -r line; do
    if [[ $line == "#" ]]; then
      if [ "$modeIn" = true ]; then
        modeIn=false
      else
   	i=$(($i+1))
        modeIn=true
      fi
    else
      if [ "$modeIn" = true ];then
        IN[$i]="${IN[$i]}""$line"
      else
        OUT[$i]="${OUT[$i]}""$line"
      fi
    fi
   done < "$FILE"
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

TESTDIR="integration-tests/passing"
EXT=".io"

### Compile and run all .rail files
TMPDIR=tests/tmp
mkdir -p $TMPDIR
fail=false
for f in "$TESTDIR"/*.rail
do
  filename="${f##*/}"
  filename="${filename%%.*}"
  if [ -f "$TESTDIR/$filename$EXT" ]
    then
      readtest "$TESTDIR/$filename$EXT"
	else
	  echo "Warning: $TESTDIR/$filename$EXT is missing."
  fi
  dist/build/SWPSoSe14/SWPSoSe14 --compile "$f" "$TMPDIR/$filename.ll"
  llvm-link "$TMPDIR/$filename.ll" src/RailCompiler/stack.ll > "$TMPDIR/$filename"
  chmod +x "$TMPDIR/$filename"
  for i in $(eval echo "{1..${#OUT[@]}}"); do
    output=$(echo -ne "${IN[$i]}" | "$TMPDIR/$filename")
    if [[ "$output" == "${OUT[$i]}" ]]; then
      echo "Passed \"$filename.rail\" with input \"${IN[$i]}\""
    else
      fail=true
      echo "ERROR testing \"$filename.rail\" with input \"${IN[$i]}\"! Expected: \"${OUT[$i]}\" got \"$output\""
    fi
  done
done

rm -r tests/tmp

### DEBUGGING:
function debugprint {
echo "IN"
for e in "${IN[@]}";do
	echo $e
done
echo "OUT"
for e in "${OUT[@]}";do
	echo $e
done
}

#debugprint

if [ "$fail" = true ];then
  exit 1
fi