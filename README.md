# 15-x17 Lab 1 - OCaml

If you have any questions, please post on Ed Discussion

## Environment Setup

The starter code uses OCaml 5.2.0, Dune 3.16 (the build system), and Menhir 3.0
(the parser generator) but the code is pretty generic and should run in older
and newer versions.  The autograder will also have these versions.

We install the following modules, although many of them are not necessary
for the starter code.

```
   opam install core dune ocamlformat menhir merlin ppx_jane ppx_deriving ppx_import utop ocaml-lsp-server odoc zarith
```

If you are using VSCode, install the [OCaml Platform VSCode extension](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform).

## Building

To build the project, run the following command:
```bash
make
```

This will build the project and create the `sax` binary in your current directory.

To clean the project directory, run the following command:
```bash
make clean
```

## Handin

We strongly recommend handing in by directly checking out a Bitbucket or Github repo.

You can also create a `lab1.zip` file for handin, running the following command:
```bash
make handin
```

### Starter Code

This is the interface you will be mostly be working with:
- [`lib/parse/ast.mli`](lib/parse/ast.mli): The abstract syntax trees (AST) for Sax types and commands

## Resources

- [Brandon Wu's From SML to OCaml Lecture Slides](https://brandonspark.github.io/ocaml/sml_to_ocaml.pdf)
  - This is a great resource for learning OCaml if you are already familiar with SML.
- [CS 3110 Course Materials](https://cs3110.github.io/textbook/cover.html)
  - This is the textbook used for Cornell's functional programming course that uses OCaml.
