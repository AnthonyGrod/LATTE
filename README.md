# Latte LLVM Compiler

## Overview

Latte is a compiler for a Java-like programming language, Latte ([language reference](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2013/Latte/description.html)), written in Haskell. The compiler generates LLVM code in Single Static Assignment (SSA) form, enabling multiple optimizations for efficient execution.

## Features

- **Written in Haskell**: The compiler leverages Haskell's strong type system and functional programming capabilities.
- **LLVM Backend**: Generates LLVM intermediate representation in SSA form for high-performance execution.
- **Optimizations**: Several optimizations are implemented to improve efficiency, including:
  - **Lowest Common Ancestor Elimination**: Reduces redundant computations by eliminating unnecessary common subexpressions.
  - **While Loop Strength Reduction**: Optimizes loop computations by replacing expensive operations with cheaper ones.
  - **Constant Propagation and Folding**: Evaluates and simplifies constant expressions at compile time.

## Usage

To build and run the compiler, use the following commands:

```bash
make
./latc <source_file>
```
