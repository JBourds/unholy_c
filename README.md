# Unholy C

The natural complement to Holy C. What makes it unholy? Well, it's C.

## Authors

[Kyrill Serdyuk](https://github.com/kyserd)

[Jordan Bourdeau](https://github.com/JBourds)

## About

This project loosely follows the book [Writing a C Compiler](https://nostarch.com/writing-c-compiler)
for writing a x86-64 C compiler using Rust, targeted for
Intel assembly syntax. This specific implementation is for a
Linux system, and uses GCC tools for preprocessing and linking.

The book comes with a
[git repo](https://github.com/nlsandler/writing-a-c-compiler-tests)
containing tests for compiler features added in each chapter.

Each chapter comes in six stages:

1. Lexing/Tokenization
2. Parsing
3. Semantic Analysis (e.g., Typechecking, label creation, etc.)
4. TACKY (Three Address Code IR Generation)
5. Code Generation
6. Code Emission

## Running

### Building

Build the compiler into `target/release/unholy_c` relative to the repository
root using the command:

`cargo b --release`

Alternatively, build/run the compiler directly with the command:

`cargo r --release -- <filepath> <optional flag>`

### Flags

By default, the compiler takes in a single input argument for the path to
compile. This will run through all of the stages, link/assemble with any other
libraries, and write the completed binary into the same directory as the input
file. By passing it a specific flag corresponding to one of the preceding
stages, it can instead write intermediary output to stdout. The flags are:

1. Lexing: `--lex`
2. Parsing: `--parse`
3. Semantic Analysis: `--validate`
4. IR Generation: `--tacky`
5. Code Generation: `--codegen`
6. Assembly: `--asm`

## Testing

The [git repo](https://github.com/nlsandler/writing-a-c-compiler-tests) linked
earlier contains the full instructions for running tests, but generally is:

`./test_compiler <Unholy C Binary Path> --chapter <n> --stage <n>`

## Developing

### Setting up Git Hooks

Git hooks are located in the `.githooks/` directory. These are configured to be
run with the command:

```git config --local core.hooksPath .githooks/```

