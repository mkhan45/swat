#!/bin/sh

rm "$1.wat"
rm "$1_opt.wat"
./sax $1
echo "wrote $1.wat"
wasm-opt --enable-bulk-memory --enable-tail-call --enable-reference-types --enable-gc -O2 "$1.wat" -S -o "$1_opt.wat"
echo "wrote $1_opt.wat"
time ./runner/target/release/runner $1_opt.wat
# ./runner/target/debug/runner $1.wat
