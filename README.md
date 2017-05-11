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
