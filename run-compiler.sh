#!/bin/sh

set -e
trap 'rm -f "$temp"' EXIT

mydir=$(dirname "$(readlink -e "$0")")


if [ $# -lt 1 ]; then
    exec 1>&2

    echo "Usage:"
    echo "$0 input-file [output-file]"
    exit 1
fi


input=$1
inputbase=$(basename "$input")
llvmoutput=${inputbase%.*}.ll

if [ $# -lt 2 ]; then
    output=${inputbase%.*}.bin
else
    shift
    output=$1
fi

echo "==> Compiling to textual LLVM IR (${llvmoutput})..."
"${mydir}/dist/build/RailCompiler/RailCompiler" -c -i "$input" -o "$llvmoutput"

echo "==> Creating binary (${output})..."
temp=$(mktemp -t railcc.XXXXX)
llvm-link "$llvmoutput" "${mydir}"/src/RailCompiler/*.ll \
    | llc -filetype=obj -o "$temp"
cc -o "$output" "$temp" -lm
