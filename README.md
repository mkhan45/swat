# 15-x17 Sax to WASM Compiler

First, build the runner with cargo:
```
cd runner && cargo build --release
```
It might require a pretty up to date Rust toolchain.

You'll also need wasm-opt from binaryen:
https://github.com/WebAssembly/binaryen


The compiler dependencies are similar to the starter code, except for the wasm submodule:

OCaml 5.2.0, Dune 3.16, Menhir 3.0, wasm from the submodule (gh:WebAssembly/spec wasm-3.0 branch), and yojson.
```
   git submodule --init
   git submodule update --remote --recursive
   opam pin add wasm ./wasm
   opam install core dune ocamlformat menhir merlin ppx_jane ppx_deriving ppx_import utop ocaml-lsp-server odoc zarith yojson js_of_ocaml js_of_ocaml-ppx
```

## Building

To build the project, run the following command:
```bash
make
```

Then, `./run.sh <test>` should build and run each test. Most of the Sax files in the test folder don't have a main proc,
so you will have more luck running the benches. The grammar also diverges a little bit from the later Sax labs since Reads
can have a single case without braces.
