#set page(paper: "a4")

#align(center)[
               #v(24pt)
               #text(size: 1.25em)[= Compiling Linear Sax to WASM]
               #v(16pt)
               CMU CS 15-817: HOT Compilation \
               Mikail Khan
               #v(16pt)
]

= Abstract
#lorem(120)

= Introduction

Sax is an intermediate representation (IR) that helps bridge the gap between
high level, richly typed languages and lower level imperative execution.
WebAssembly (WASM) is a bytecode instruction set focused on speed, security, and portability.
It is an appealing compilation target because it can be run quickly and securely
on many platforms, including embedded devices and web browsers, and is used
in industry for accelerating web performance or running untrusted code.

remember to include sax support for ints

=== WASM Structure

WASM's structure and execution are largely similar to well-known instruction sets like
x86 or Java bytecode, with the major difference that all its control flow is
structured. It is primarily a stack VM; instructions operate on some combination of
bytecode immediates and stack operands. There are no stack manipulation instructions.
Instead, there is a per-function set of local values which can be accessed through static
indices. 

WASM also supports both a manually managed heap and a garbage collector.

=== WASM Runtimes

WASM can be run...

Wasmtime is a runtime...

#lorem(40)

=== Sax Integers

#lorem(40)

=== Compilation Stages

The compiler and runtime have several components. First, the compiler
does a simple pass over the Sax, generating a simple stack-based sequential IR
and doing a simple static analysis. Then, it does another pass over the stack-based IR
to generate WASM instructions. Finally, the compiler expects the WASM module to be optimized by
`wasm-opt`. It should not be strictly necessary, but there seem to be differences in how Wasmtime 
and `wasm-opt` validate modules, so output directly from the compiler often does not run in Wasmtime 
without using `wasm-opt` first.

= Implementation

=== Core Translation

The translation from Sax to WASM is guided by a few principles. First, the destination of the
current translated command is just the top of the stack. Thus, translating a metavariable writing
to destination `d` results in a sequence of WASM instructions that result in `d` on the top of the
stack.

Next about stack val layout

=== Locals

=== Allocation

=== Tail Calls

=== Printing

=== Optimizations

=== Runtime

#lorem(480)

= Evaluation

#lorem(480)

= Related Work

#lorem(240)

= Conclusion

#lorem(120)
