# mur
mur is an interpreter and editor for simple MapReduce language. mur is just acronym for Map Und Reduce. 

The language has the following grammar:
```
expr ::= expr op expr | (expr) | identifier | { expr, expr } | number |
map(expr, identifier -> expr) | reduce(expr, expr, identifier identifier -> expr)
op ::= + | - | * | / | ^
stmt ::= var identifier = expr | out expr | print "string"
program ::= stmt | program stmt
```

Run the tests with enabled coverage:
```
$ sbt clean coverage test
```
To disable coverage directly in build, use:
```
coverageEnabled := false
```
To generate the coverage reports run
```
$ sbt coverageReport
```
It creates the file with reports: target/scala-2.12/scoverage-report/index.htm

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