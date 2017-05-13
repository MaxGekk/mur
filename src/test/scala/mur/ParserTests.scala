package mur

import org.scalatest.{FreeSpec, Matchers}

class ParserTests extends FreeSpec with Matchers {
  "Parsing of " - {
    " integer" in {
      assert(Parsers.parse("var i = 42") == VarDef("i", Literal(42)))
    }
    " double" in {
      assert(Parsers.parse("out 3.14") == Out(Literal(3.14)))
    }
    " print" in {
      assert(Parsers.parse(""" print "Hello" """) == Print("Hello"))
    }
    "a simple sum" in {
      assert(Parsers.parse("var x = 2 + 3") == VarDef("x", Plus(Literal(2), Literal(3))))
    }
    "a simple minus of ints" in {
      assert(Parsers.parse("out 2 - 3") == Out(Minus(Literal(2), Literal(3))))
    }
    "minus of doubles" in {
      assert(Parsers.parse("out 2.78 - 3.14") == Out(Minus(Literal(2.78), Literal(3.14))))
    }
    "a simple multiplication" ignore {
      assert(Parsers.parse("out 2 * 3") == Out(Mul(Literal(2), Literal(3))))
    }
    "a simple div" in {
      assert(Parsers.parse("out -1 / 4") == Out(Div(Literal(-1), Literal(4))))
    }
    "a simple pow" in {
      assert(Parsers.parse("var j = (-1) ^ 10") ==
        VarDef("j", Pow(Brackets(Literal(-1)), Literal(10)))
      )
    }
    "identifier" in {
      assert(Parsers.parse("out abc123") == Out(Id("abc123")))
    }
    "a sequence of ints" in {
      assert(Parsers.parse("var sequence = {1, 2}") ==
        VarDef("sequence", Sequence(Literal(1), Literal(2)))
      )
    }
    "map" ignore {
      assert(Parsers.parse("out map({1, 10}, i -> 2 * i)") ==
        Out(MapSeq(
          Sequence(Literal(1), Literal(10)),
          Id("i"),
          Mul(Literal(2), Id("i"))
        ))
      )
    }
    "reduce" ignore {
      assert(Parsers.parse("reduce(sequence, 0, x y -> x + y)") ==
        ReduceSeq(
          Id("sequence"), Literal(0), Id("x"), Id("y"),
          Plus(Id("x"), Id("y"))
        )
      )
    }
  }
  "Parsing a complex expression of " - {
    " (2 * 3) * (4 + 5)" in {

      assert(Parsers.parse("out (2 * 3) * (4 + 5)") ==
        Out(Mul(
          Brackets(Mul(
            Literal(2),
            Literal(3)
          )),
          Brackets(Plus(
            Literal(4),
            Literal(5)
          ))
        ))
      )
    }
    "1 + 2 * 3" in {

      assert(Parsers.parse("out 1 + 2 * 3") ==
        Out(Plus(
          Literal(1),
          Mul(Literal(2), Literal(3))
        ))
      )
    }
  }
}
