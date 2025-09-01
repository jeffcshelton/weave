# Weave

The Weave programming language is a work-in-progress language that enables the
programmer to import source files from a wide selection of other languages and
to use the definitions and values in Weave files.

## Usage

At the moment, Weave is purely a JIT-compiled language. To run a Weave file,
install the Weave compiler and run:

```
$ weave <file.w>
```

The file can be named anything, but for a project with multiple files that are
all executed as one program, the convention is to call the entry file `main.w`.
The paths of any Weave files imported by the entry file or its dependencies will
be evaluated immediately upon running, including any imports of source files
written in different languages.

### Executables

Once Weave is out of alpha, the compiler will support generation of Weave
executables. Weave code and code from other compiled languages will be compiled
and embedded directly into the binary, while languages that are JIT-compiled or
interpreted will have their source code embedded into the binary along with an
JIT-compiler or interpreter as needed, and they will be evaluated at runtime.

All import paths within JIT or interpreted languages will be changed at compile
time so that the final executable is self-contained.

## Grammar

A formal specification of the Weave grammar is laid out in
[grammar.md](./grammar.md).
