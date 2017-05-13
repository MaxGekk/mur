package mur

import org.scalatest.{FreeSpec, Matchers}

class ParserTests extends FreeSpec with Matchers {
  "Parsing of " - {
    " integer" in {
      assert(Parsers.parseExpr("42") == Literal(42))
    }
    "a simple sum" in {
      assert(Parsers.parseExpr("2 + 3") == Plus(Literal(2), Literal(3)))
    }
    "a simple multiplication" in {
      assert(Parsers.parseExpr("2 * 3") == Mul(Literal(2), Literal(3)))
    }
    "a simple div" in {
      assert(Parsers.parseExpr("-1 / 4") == Div(Literal(-1), Literal(4)))
    }
    "a simple pow" in {
      assert(Parsers.parseExpr("(-1) ^ 10") == Pow(Literal(-1), Literal(10)))
    }
  }
  "Parsing a complex expression of " - {
    " integers" in {

      assert(Parsers.parseExpr("(2 * 3) * (4 + 5)") ==
        Mul(
          Mul(
            Literal(2),
            Literal(3)),
          Plus(
            Literal(4),
            Literal(5)
          )
        )
      )
    }
  }
}
