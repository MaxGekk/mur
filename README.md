# MuR
MuR is an interpreter and editor for simple MapReduce language. MuR is just acronym for Map Und Reduce. 

The language has the following grammar:
```
expr ::= expr op expr | (expr) | identifier | { expr, expr } | number |
map(expr, identifier -> expr) | reduce(expr, expr, identifier identifier -> expr)
op ::= + | - | * | / | ^
stmt ::= var identifier = expr | out expr | print "string"
program ::= stmt | program stmt
```
## Editor
The following command creates jar file with MuR Editor:
```
$ sbt assembly
[info] Packaging /Users/dev/proj/mur/target/scala-2.12/mur-editor.jar ...
```
To run the editor:
```
java -jar /Users/dev/proj/mur/target/scala-2.12/mur-editor.jar
```

## Tests
Run the tests with enabled coverage:
```
$ sbt clean coverage test
```
To enable/disable coverage directly in build, change this:
```
coverageEnabled := false
```
To generate the coverage reports run
```
$ sbt coverageReport
```
It creates the file with reports: target/scala-2.12/scoverage-report/index.htm

## Example
Parsing and interpretation of the example:
```
var n = 500
var sequence = map({0, n}, i -> (-1)^i / (2 * i + 1))
var pi = 4 * reduce(sequence, 0, x y -> x + y)
print "pi = "
out pi
```
just launch:
```
$ sbt "run-main mur.Main"
[info] Running mur.Main 
pi = 3.143588659585789
[success] Total time: 1 s, completed May 14, 2017 6:21:27 PM
```

# API
There are two main methods:
- Parsers.parse converts a program text to its intermediate representation (IR). IR is defined in IR.scala and Expr.scala
- Interpreter.run takes an IR and executes it step-by-step, and produces a sequence of strings as a result of the out and print statements  