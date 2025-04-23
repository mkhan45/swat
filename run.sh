#!/bin/sh

./sax $1
echo "wrote $1.wat"
wasm-opt --enable-bulk-memory --enable-tail-call -O2 "$1.wat" -S -o "$1_opt.wat"
echo "wrote $1_opt.wat"
time ./runner/target/release/runner $1_opt.wat
# ./runner/target/debug/runner $1.wat
