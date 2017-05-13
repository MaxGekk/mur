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
    "a simple multiplication" in {
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
    "map" in {
      assert(Parsers.parse("var m = map({0, 10}, i -> i + 1)") ==
        VarDef("m", MapSeq(
          Sequence(Literal(0), Literal(10)),
          Id("i"),
          Plus(Id("i"), Literal(1))
        ))
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
    "2 / 3 ^ 4" in {

      assert(Parsers.parse("out 2 / 3 ^ 4") ==
        Out(Div(
          Literal(2),
          Pow(Literal(3), Literal(4))
        ))
      )
    }
    "2 ^ 4 - 3 * 8" in {

      assert(Parsers.parse("out 2 ^ 4 - 3 * 8") ==
        Out(Minus(
          Pow(Literal(2), Literal(4)),
          Mul(Literal(3), Literal(8))
        ))
      )
    }
  }
}
