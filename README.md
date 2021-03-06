You can build using Stack or by `make all`. Caution: the latter will download stack and use it to
set up the program and then copy binary to local directory.

Basic instruction of running the language interpreter:
  * `./interpreter <filename>` will run from the file
  * `./interpreter` will run from stdin

Code examples are placed in good (positive) and bad (negative) directories. You can just run them
with interpreter. Positive examples (they're a test suite too) should all result in True.

Language features:
  * strongly typed lambda calculus
  * type inference using H-M algorithm W (with polymorphism)
  * type annotations
  * explicite laziness
  * if-then-else and operators being only syntactic sugar for calling builtin functions
  * "undefined" explicite-fail primitive
  * Syntax for defining your own operators
  * some light syntactic sugar (ie. multivariable lambdas)
  * error handling parse-, typecheck- and run-time with file/line number/column references

Non-implemented features:
  * classes, monads and IO

Standard library implemented in `stdlib.ffl`.


Match expression syntax is non-obvius. There is:
```
match expression with
  Constructor1 param1 param2 -> result1 | Constructor2 -> result2 end
```
where end is optional and used to force finish match expression, very useful in case of nested
matches which would be unimplementable otherwise.
