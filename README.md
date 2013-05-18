
# Fedit - Text editor for FOUL


## Complete

* Rewrote the parser using Parsec. It now output useful error messages & positions to help find errors
* Rewrote the interpreter to handle errors more gracefully
* Check the program before interpreting for duplicate method declarations and functions declarations that take diffrent numbers of paramaters
* Added a module import system to the parser. Users can define their own functions in seperate files and import them in.
* Implemented some standard library modules. These modules are not part of the interpreter, they're foul files stored in the std directory of this repository. Cabals Path_fedit module is used to access them on the host system at runtime. 
	* import std/math: Add, Subtract, Multiply, Divide, Equals, More Than, More Than Equals, Less Than, Less Than Equals, Square, Fibonacci & Infinity (Negative numbers aren't supported and causes a crash)
	* import std/bool: Conditional Application (cond), and, or & not
	* import std/test: Module used for testing. Its used to implement tests for std/bool & std/math (tests are in tests/FOUL)
* Add basic integer support to the parser. Instead of writing S(S(S(Z))) you can just write 3 (both in patterns and expressions)

## Language TODO

* Excpetion throwing
* Pretty Print Results
* Store a (function -> module) mappign so errors can be matched to a module
* Make lists nicer to work with
* Strings? (Need a char data type)

## Editor TODO

* Implement basic syntax highlighting
* Error highlighting
* Interactive evaluation (ghci style)
* Save / Load files