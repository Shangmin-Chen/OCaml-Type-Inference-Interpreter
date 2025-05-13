# OCaml Type Inference Interpreter

## Overview

This project implements an interpreter for a subset of OCaml, developed as part of the CAS CS 320: Principles of Programming Languages course at Boston University (Spring 2025). The interpreter performs parsing, constraint-based type inference, and expression evaluation using big-step operational semantics. It supports a rich set of language constructs, including literals, options, lists, pairs, and recursive functions, with robust error handling for type errors and runtime exceptions.

Written in OCaml, the interpreter is built using a Dune project structure, with core logic in `lib/interp3.ml`. This project showcases skills in functional programming, type systems, and interpreter design.

## Features

- **Parsing**: Converts input programs into an internal `expr` representation, respecting operator precedence and associativity.
- **Type Inference**: Infers principal type schemes using a constraint-based system with unification and substitution.
- **Evaluation**: Executes expressions to produce values (e.g., integers, closures, lists) via big-step semantics.
- **Error Handling**: Manages runtime errors like division by zero, assertion failures, and invalid function comparisons.
- **Testing**: Includes a test suite in `examples.ml` to validate functionality, such as the `sum_of_squares` example.

## Supported Constructs

The interpreter supports the following OCaml constructs:
- **Literals**: `()`, `true`, `false`, integers, floats
- **Options**: `None`, `Some e`, option matching
- **Lists**: `[]`, `e1 :: e2`, list matching
- **Pairs**: `(e1, e2)`, pair matching
- **Variables**: Polymorphic variables with type schemes
- **Annotations**: `(e : τ)`
- **Assertions**: `assert e`, `assert false`
- **Operators**: Arithmetic (`+`, `-`, `*`, `/`, `+.`, `-.`, `*.`, `/.`, `mod`), comparisons (`<`, `<=`, `>`, `>=`, `=`, `<>`), logical (`&&`, `||`), cons (`::`)
- **Bindings**: `let x = e1 in e2`, `let rec f = e1 in e2` for recursive functions

**Operator Precedence** (highest to lowest):
| Operators         | Associativity |
|-------------------|---------------|
| Function application | Left       |
| `*`, `/`, `mod`, `*.`, `/.` | Left |
| `+`, `-`, `+.`, `-.` | Left       |
| `::`              | Right      |
| `<`, `<=`, `>`, `>=`, `=`, `<>` | Left |
| `&&`              | Right      |
| `||`              | Right      |

## Implementation Details

The project is implemented in `lib/interp3.ml`, with supporting types and utilities in `lib/utils/utils.ml`.

### Parsing
- Utilizes `Parser.prog` and `Lexer.read` to parse input strings into a `prog` structure.
- Extends the provided parser to handle the full grammar, desugaring constructs like pattern matching into `expr` representations.

### Type Inference
Key functions include:
- **`principle_type ty cs`**: Computes the principal type scheme by unifying constraints (`unify`) and applying substitutions (`ty_subst`), using `VarSet` to quantify free variables.
- **`type_of ctxt e`**: Infers an expression’s type scheme, generating constraints (e.g., `(t1, TBool)` for conditionals) and invoking `principle_type`.
- **`is_well_typed p`**: Verifies that a program’s let-bindings are well-typed, building a static context with `Env.add`.

The system supports typing rules for literals, options, lists, pairs, and recursive functions, using `gensym` for fresh type variables and `instantiate` for polymorphism.

### Evaluation
- **`eval_expr env e`**: Evaluates expressions to values (e.g., `VInt`, `VClos`, `VList`) following big-step semantics.
- Supports closures (`VClos`) for functions, list cons (`VList (v_hd :: v_tl)`), and pattern matching for options and pairs.
- Handles exceptions:
  - `DivByZero`: Raised for division or modulus by zero.
  - `AssertFail`: Triggered by failed assertions.
  - `CompareFunVals`: Thrown for invalid comparisons involving closures.

### Utilities
- `unify`: Resolves constraint lists, handling compound types like `TList` and `TPair`.
- `ty_subst`: Applies substitutions to types, supporting recursive types like `TFun`.
- `free` and `occurs`: Manage type variables during unification.

## Project Structure

- `lib/interp3.ml`: Core implementation of parsing, type inference, and evaluation.
- `lib/utils/utils.ml`: Defines types (`ty`, `expr`, `value`, etc.) and utilities (`gensym`, `VarSet`).
- `examples.ml`: Test cases, including the `sum_of_squares` example from the specification.
- Dune build system for compilation and testing.

## Setup and Installation

### Prerequisites
- OCaml 4.14 or later
- Dune 3.0 or later
- Git

### Installation
1. Clone the repository:
   ```bash
   git clone https://github.com/your-username/type-inference-ocaml.git
   cd type-inference-ocaml
   ```
2. Build the project:
   ```bash
   eval $(opam env)
   dune build
   ```

### Running Tests
Execute the test suite:
```bash
dune test
```
For debugging, extract test cases from `examples.ml` and run them in `dune utop`. The suite includes examples like:
```ocaml
let sum_of_squares x y =
    let x_squared = x * x in
    let y_squared = y * y in
    x_squared + y_squared
let _ = assert (sum_of_squares 3 (-5) = 34)
```

## Usage

Use the `interp` function to parse, type-check, and evaluate programs:
```ocaml
let input = "let x = 3 in x + 2" in
match interp input with
| Ok value -> (* Returns VInt 5 *)
| Error ParseError -> (* Handles parse errors *)
| Error TypeError -> (* Handles type errors *)
```

## Notes

- The implementation preserves the course’s skeleton code (e.g., Dune files, `utils.ml`) and adheres to exact function signatures in `interp3.ml`.
- The test suite in `examples.ml` validates core functionality but may not cover all edge cases. Additional manual testing was performed for robustness.
- Helper functions in `type_of` and `eval_expr` reduce code duplication, improving maintainability.

## Acknowledgments

- CAS CS 320 course staff for providing the project specification and skeleton code.
- Test cases in `examples.ml` for validating the interpreter’s correctness.