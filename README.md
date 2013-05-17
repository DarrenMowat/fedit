
# Fedit - Text editor for FOUL


## Complete

* Rewrote the parser using Parsec. It now output useful error messages & positions to help find errors
* Rewrote the interpreter to handle errors more gracefully
* Check the program before interpreting for duplicate method declarations and functions declarations that take diffrent numbers of paramaters
* Added a module import system to the parser. Users can define their own functions in seperate files and import them in.
* Implemented some standard library modules
	* import std/math: Add, Subtract, Multiply, Divide, Square, Fibonacci & Infinity 
	* import std/bool: Conditional Application (cond), and, or & not
* Add basic integer support to the parser. Instead of writing S(S(S(Z))) you can just write 3 (both in patterns and expressions)

## Language TODO

* Catch runtime errors better
* Excpetion throwing
* Pretty Print Results
* Store a (function -> module) mappign so errors can be matched to a module

## Editor TODO

* Implement basic syntax highlighting
* Error highlighting
* Interactive evaluation (ghci style)
* Save / Load files