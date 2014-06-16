#!/bin/bash

# Usage info
function show_help {
cat << EOF
Usage: ${0##*/} [-hvl] [-e/d TEST] [TEST]...
Without arguments the script runs all enabled tests.
When a test name is given then run this test.

-h          display this help and exit
-e/d TEST   Enable/Diasble the specified test.
-l	    List all tests and their status.
-v          verbose mode. Can be used multiple times for increased
	    verbotisty.
EOF
}

verbose=0
test1=""

OPTIND=1


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

### Function to correctly call the LLVM interpreter
function do_lli {
  # On some platforms, the LLVM IR interpreter is not called "lli", but
  # something like "lli-x.y", where x.y is the LLVM version -- there may be
  # multiple such binaries for different LLVM versions.
  # Instead of trying to find the right version, we currently assume that
  # such platforms use binfmt_misc to execute LLVM IR files directly (e. g. Ubuntu).
  if command -v lli >/dev/null; then
      lli "$@"
  else
      "$@"
  fi
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
for f in "$TESTDIR"/*.rail; do
  dontrun=false
  filename="${f##*/}"
  filename="${filename%%.*}"
  if [ -f "$TESTDIR/$filename$EXT" ]
    then
      readtest "$TESTDIR/$filename$EXT"
    else
      fail=true
      dontrun=true
      echo "ERROR testing: \"$filename.rail\". $EXT-file is missing."
  fi
  errormsg=$(dist/build/SWPSoSe14/SWPSoSe14 -c -i "$f" -o "$TMPDIR/$filename.ll" 2>&1) \
  	  && llvm-link "$TMPDIR/$filename.ll" src/RailCompiler/stack.ll > "$TMPDIR/$filename" \
	  && chmod +x "$TMPDIR/$filename" || { 
            dontrun=true
	    if [[ "$errormsg" == "${OUT[1]}" ]]; then
	      echo "Passed expected fail \"$filename.rail\"."
	    else
              fail=true
	      echo "ERROR compiling/linking \"$filename.rail\" with error: \"$errormsg\""
            fi
	}
  if [ "$dontrun" = false ]; then
    for i in $(eval echo "{1..${#OUT[@]}}"); do
      #Really ugly: bash command substitution eats trailing newlines so we need to add a terminating character and then remove it again.
      output="$(echo -ne "${IN[$i]}" | do_lli "$TMPDIR/$filename";echo x)"
      output="${output%x}"
      #Convert all actual newlines to \n
      output="${output//$'\n'/\\n}"
      if [[ "$output" == "${OUT[$i]}" ]]; then
        echo "Passed \"$filename.rail\" with input \"${IN[$i]}\""
      else
        fail=true
        echo "ERROR testing \"$filename.rail\" with input \"${IN[$i]}\"! Expected: \"${OUT[$i]}\" got \"$output\""
      fi
    done
  fi
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
