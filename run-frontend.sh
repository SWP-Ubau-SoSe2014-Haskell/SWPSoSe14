#!/bin/sh

set -e
trap 'rm -f "$temp"' EXIT

mydir=$(dirname "$(readlink -e "$0")")


if [ $# -lt 1 ]; then
    exec 1>&2

    echo "Usage:"
    echo "$0 input-file csv-output-file"
    exit 1
fi


input=$1
inputbase=$(basename "$input")
llvmoutput=${inputbase%.*}.ll

output=$2


#echo "$1"
#echo "$2"


echo "==> Generating AST-File (${csvOut})..."
"${mydir}/dist/build/RailCompiler/RailCompiler" --exportAST="$output" -i "$input"

