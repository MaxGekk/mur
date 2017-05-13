package mur

import org.scalatest.{FreeSpec, Matchers}

class ParserTests extends FreeSpec with Matchers {
  "Parsing of " - {
    " integer" in {
      assert(Parsers.parseExpr("42") == Literal(42))
    }
    " double" in {
      assert(Parsers.parseExpr("3.14") == Literal(3.14))
    }
    "a simple sum" in {
      assert(Parsers.parseExpr("2 + 3") == Plus(Literal(2), Literal(3)))
    }
    "a simple minus of ints" in {
      assert(Parsers.parseExpr("2 - 3") == Minus(Literal(2), Literal(3)))
    }
    "minus of doubles" in {
      assert(Parsers.parseExpr("2.78 - 3.14") == Minus(Literal(2.78), Literal(3.14)))
    }
    "a simple multiplication" in {
      assert(Parsers.parseExpr("2 * 3") == Mul(Literal(2), Literal(3)))
    }
    "a simple div" in {
      assert(Parsers.parseExpr("-1 / 4") == Div(Literal(-1), Literal(4)))
    }
    "a simple pow" in {
      assert(Parsers.parseExpr("(-1) ^ 10") == Pow(Brackets(Literal(-1)), Literal(10)))
    }
    "identifier" in {
      assert(Parsers.parseExpr("abc123") == Id("abc123"))
    }
    "a sequence of ints" in {
      assert(Parsers.parseExpr("{1, 2}") == Sequence(Literal(1), Literal(2)))
    }
    "map" ignore {
      assert(Parsers.parseExpr("map({1, 10}, i -> 2 * i)") ==
        MapSeq(
          Sequence(Literal(1), Literal(10)),
          Id("i"),
          Mul(Literal(2), Id("i"))
        )
      )
    }
    "reduce" in {
      assert(Parsers.parseExpr("reduce(sequence, 0, x y -> x + y)") ==
        ReduceSeq(
          Id("sequence"), Literal(0), Id("x"), Id("y"),
          Plus(Id("x"), Id("y"))
        )
      )
    }
  }
  "Parsing a complex expression of " - {
    " integers" in {

      assert(Parsers.parseExpr("(2 * 3) * (4 + 5)") ==
        Mul(
          Brackets(Mul(
            Literal(2),
            Literal(3)
          )),
          Brackets(Plus(
            Literal(4),
            Literal(5)
          ))
        )
      )
    }
  }
}
