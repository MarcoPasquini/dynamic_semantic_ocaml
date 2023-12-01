# Interpreter in Ocaml

The implementation focuses solely on dynamic semantics (assuming that a lexer, parser, and static type checker already exist).
In this language, every program is an expression that produces a value. Expressions include integers, booleans, strings, pairs, and identifiers.
Additionally, the implementation covers arithmetic operations on integers, logical operations on integers and booleans, string operations, constructs such as if-then-else, declarations (of variables and functions, including recursive functions and function application).
Values consist of primitive values, with the addition of pairs and closures, representing the implementation of functions with static scoping (remembering the environment at the time of declaration).
For handling declarations, an environmental model is used instead of a substitution model for its increased efficiency.
Example of evaluation:
eval expression emptyenv
//where expression is something like Let("x", Prod(EInt(5), EInt(3)), Sum(EInt(7), Den("x"))
