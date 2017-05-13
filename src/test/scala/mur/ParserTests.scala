package mur

import org.scalatest.{FreeSpec, Matchers}

class ParserTests extends FreeSpec with Matchers {
  "Parsing of " - {
    " integer" in {
      assert(Parsers.parseExpression("42") == Literal(42))
    }
  }
}
