package mur

import org.scalatest.{FreeSpec, Matchers}

class ParserTests extends FreeSpec with Matchers {
  "Parsing of " - {
    " integer" in {
      Parsers.parse("var i = 42").right.get shouldBe
        Program(Seq(
          VarDef("i", Literal(42))
        ))
    }
    " double" in {
      Parsers.parse("out 3.14").right.get shouldBe Program(Seq(Out(Literal(3.14))))

    }
    " print" in {
      Parsers.parse(""" print "Hello" """).right.get shouldBe Program(Seq(Print("Hello")))
    }
    "a simple sum" in {
      Parsers.parse("var x = 2 + 3").right.get shouldBe
        Program(Seq(
          VarDef("x", Plus(Literal(2), Literal(3)))
        ))
    }
    "a simple minus of ints" in {
      Parsers.parse("out 2 - 3").right.get shouldBe
        Program(Seq(
          Out(Minus(Literal(2), Literal(3)))
        ))
    }
    "minus of doubles" in {
      Parsers.parse("out 2.78 - 3.14").right.get shouldBe
        Program(Seq(
          Out(Minus(Literal(2.78), Literal(3.14)))
        ))
    }
    "a simple multiplication" in {
      Parsers.parse("out 2 * 3").right.get shouldBe
        Program(Seq(
          Out(Mul(Literal(2), Literal(3)))
        ))
    }
    "a simple div" in {
      Parsers.parse("out -1 / 4").right.get shouldBe
        Program(Seq(
          Out(Div(Literal(-1), Literal(4)))
        ))
    }
    "a simple pow" in {
      Parsers.parse("var j = (-1) ^ 10").right.get shouldBe
        Program(Seq(
          VarDef("j", Pow(Brackets(Literal(-1)), Literal(10)))
        ))
    }
    "identifier" in {
      Parsers.parse("out abc123").right.get shouldBe
        Program(Seq(Out(Id("abc123"))))
    }
    "a sequence of ints" in {
      Parsers.parse("var sequence = {1, 2}").right.get shouldBe
        Program(Seq(
          VarDef("sequence", Sequence(Literal(1), Literal(2)))
        )
      )
    }
    "map" in {
      Parsers.parse("var m = map({0, 10}, i -> i + 1)").right.get shouldBe
        Program(Seq(
          VarDef("m", MapSeq(
            Sequence(Literal(0), Literal(10)),
            Id("i"),
            Plus(Id("i"), Literal(1))
          ))
        )
      )
    }
    "map over sequence" in {
      Parsers.parse("var sequence = map({0, n}, i -> (-1)^i / (2 * i + 1))").right.get shouldBe
        Program(Seq(VarDef("sequence",
          MapSeq(
            Sequence(Literal(0), Id("n")),
            Id("i"),
            Div(
              Pow(Brackets(Literal(-1)), Id("i")),
              Brackets(Plus(Mul(Literal(2), Id("i")), Literal(1)))
            )
          )
        ))
      )
    }
    "reduce" in {
      Parsers.parse("var r = 4 * reduce(sequence, 0, x y -> x + y)").right.get shouldBe
        Program(Seq(
          VarDef("r",
            Mul(Literal(4),
              ReduceSeq(
                Id("sequence"), Literal(0),
                Id("x"), Id("y"),
                Plus(Id("x"), Id("y"))
              )
            )
          )
        )
      )
    }
  }
  "Parsing a complex expression of " - {
    " (2 * 3) * (4 + 5)" in {

      Parsers.parse("out (2 * 3) * (4 + 5)").right.get shouldBe
        Program(Seq(Out(Mul(
          Brackets(Mul(
            Literal(2),
            Literal(3)
          )),
          Brackets(Plus(
            Literal(4),
            Literal(5)
          ))
        )))
      )
    }
    "1 + 2 * 3" in {

      Parsers.parse("out 1 + 2 * 3").right.get shouldBe
        Program(Seq(Out(Plus(
          Literal(1),
          Mul(Literal(2), Literal(3))
        )))
      )
    }
    "2 / 3 ^ 4" in {

      Parsers.parse("out 2 / 3 ^ 4").right.get shouldBe
        Program(Seq(Out(Div(
          Literal(2),
          Pow(Literal(3), Literal(4))
        )))
      )
    }
    "2 ^ 4 - 3 * 8" in {

      Parsers.parse("out 2 ^ 4 - 3 * 8").right.get shouldBe
        Program(Seq(Out(Minus(
          Pow(Literal(2), Literal(4)),
          Mul(Literal(3), Literal(8))
        )))
      )
    }
  }
  "Parsing of multiple lines " - {
    "vars" in {
      val prog =
        """
          |print "pi = "
          |out pi
        """.stripMargin
      Parsers.parse(prog).right.get shouldBe Program(Seq(Print("pi = "), Out(Id("pi"))))
    }
  }
  "Parsing of multiple lines " - {
    "wrong text" in {
      val prog =
        """
          |out a +-
        """.stripMargin
      Parsers.parse(prog).isRight shouldBe false
    }
  }
}