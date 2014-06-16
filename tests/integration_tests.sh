#!/bin/bash

### Usage info
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

### Function to get the correct test name for a file.
function get_name {
  filename="${1##*/}"
  filename="${filename%%.*}"
  echo "$filename"
}

### Function to run a single test
function run_one {
  dontrun=false
  filename=$(get_name "$1")
  if [ -f "$TESTDIR/$filename$EXT" ]
    then
      readtest "$TESTDIR/$filename$EXT"
    else
      fail=true
      dontrun=true
      echo -e "`$red`ERROR`$NC` testing: \"$filename.rail\". $EXT-file is missing."
  fi
  errormsg=$(dist/build/SWPSoSe14/SWPSoSe14 -c -i "$1" -o "$TMPDIR/$filename.ll" 2>&1) \
  	  && llvm-link "$TMPDIR/$filename.ll" src/RailCompiler/stack.ll > "$TMPDIR/$filename" \
	  && chmod +x "$TMPDIR/$filename" || { 
            dontrun=true
	    if [[ "$errormsg" == "${OUT[1]}" ]]; then
	      echo -e "`$green`Passed`$NC` expected fail \"$filename.rail\"."
	    else
              fail=true
	      echo -e "`$red`ERROR`$NC` compiling/linking \"$filename.rail\" with error: \"$errormsg\""
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
        echo -e "`$green`Passed`$NC` \"$filename.rail\" with input \"${IN[$i]}\""
      else
        fail=true
        echo -e "`$red`ERROR`$NC` testing \"$filename.rail\" with input \"${IN[$i]}\"! Expected: \"${OUT[$i]}\" got \"$output\""
      fi
    done
  fi
}

### Function to compile and run all .rail files
function run_all {
  for f in "$TESTDIR"/*.rail; do
    run_one "$f"
  done
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


### Directory magic, so our cwd is the project home directory.
unset CDPATH
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
cd "$DIR/.."

### Define Terminal Colours
red="eval tput setaf 1; tput bold"
green="eval tput setaf 2; tput bold"
NC="tput sgr 0" # No Color

### Parse commandline options.
verbose=0
test1=""
enable1=""
disable=""

OPTIND=1
while getopts "hvle:d:" opt; do
  case "$opt" in
    h)
      show_help
      exit 0
      ;;
    v)
      verbose=$(($verbose + 1))
      ;;
    l)
      list=true
      ;;
    e)
      enable1=$OPTARG
      ;;
    d)
      disable=$OPTARG
      ;;
    '?')
      show_help >&2
      exit 1
      ;;
  esac
done
shift "$((OPTIND-1))" # Shift off the options and optional --.
test1="$1"

### Checking for incompatible options.
[[ -n $list ]] && count=$(($count + 1))
[[ -n $disable ]] && count=$(($count + 1))
[[ -n $enable1 ]] && count=$(($count + 1))
if (( $count > 1 )); then
  echo "Only specify one of -l, -e, -d."
  exit 1
fi

### Main function.
TESTDIR="integration-tests/run"
EXT=".io"
if [ -n "$list" ]; then
  echo -ne "`$green`Tests to run:`$NC`\n\n"
  for file in "$TESTDIR"/*.rail;do
    echo $(get_name $file)
  done
  echo -ne "\n\n`$red`Disabled tests:`$NC`\n\n"
  for file in "$TESTDIR"/../*.rail;do
    if [ ! -f "$TESTDIR"/`basename "$file"` ];then
      echo $(get_name $file)
    fi
  done
else
  TMPDIR=tests/tmp
  mkdir -p $TMPDIR
  fail=false
  if [ -n "$test1" ];then
    run_one $test1
  else
    run_all
  fi
  rm -r tests/tmp
fi

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
