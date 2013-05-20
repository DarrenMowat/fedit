# Foul EDITor

CS410 Final Assignment

Darren Mowat

200911909

## Things added to the FOUL Language

* Rewrote the parser using Parsec. It now output useful error messages & positions to help find errors
* Rewrote the interpreter to handle errors more gracefully
* Check the program before interpreting for duplicate method declarations and functions declarations that take different numbers of parameters
* Added single line comments to the language (-- Comment Goes Here)
* Added a module import system to the parser. Users can define their own functions in separate files and import them in.
* Implemented some standard library modules. These modules are not part of the interpreter, they're foul files stored in the std directory of this repository. Cabals Path_fedit module is used to access them on the host system at runtime. 
	* import std/math: Add, Subtract, Multiply, Divide, Equals, More Than, More Than Equals, Less Than, Less Than Equals, Square, Fibonacci & Infinity (Negative numbers aren't supported and causes a crash)
	* import std/bool: Conditional Application (cond), and, or & not
	* import std/test: Module used for testing. Its used to implement tests for std/bool & std/math (tests are in tests/FOUL)
* Add basic integer support to the parser. Instead of writing S(S(S(Z))) you can just write 3 (both in patterns and expressions)
* Pretty Print Values (VC "T" [] -> T, VC "S" [VC "Z" []] -> 1)

## Things added to the editor

* Fixed handleKey bug which loops forever and uses 100% CPU
* Evaluate the main function of the file on exit
* Save the file on exit
* Line Numbers
* Added a way to evaluate expressions within the files. This is available using the following syntax
    * -- eval EXPR
    * -- eval add(7, 8)
    * The answer will then be inserted into the comment -- eval add(7, 8) -> 15
    * Evaluation is triggered by typing Ctrl+e
* Display parser errors at the bottom of the editor window
* In line evaluation errors as comments

## Installation

FOUL & Fedit are packed using Cabal as they have some external dependencies that would be hard to manage with MakeFiles.
Cabal will build 2 seperate binarys when run

    * foul - FOUL parser and evaluator
    * fedit - Foul EDITor 

Get a copy of this repository either by 

    git clone https://github.com/DarrenMowat/fedit.git
    
Or by downloading it as a zip file from https://github.com/DarrenMowat/fedit/archive/master.zip

    cd fedit
    cabal configure
    cabal install
   
The fedit & foul binaries should now be available on your PATH somewhere 

## Sample FOUL Files

There is a modified implementation of the sorting function in sample/mysort.foul. 
It utilises the standard library modules, comments and the use of integers as expressions.

There are also some test files designed to test the standard library functions in tests/FOUL/*.

The standard library modules are defined in std/*. These show an example of using integers as pattern variables

## Screenshots

* Parse Error

![ParseError](https://raw.github.com/DarrenMowat/fedit/master/web-doc/perror.png)

* Evaluating fib(20)

![Blackbox](https://raw.github.com/DarrenMowat/fedit/master/web-doc/eval.png)

* Maths standard library

![Blackbox](https://raw.github.com/DarrenMowat/fedit/master/web-doc/mathandstuff.png)







