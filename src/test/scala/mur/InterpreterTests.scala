package mur

import org.scalatest.{FreeSpec, Matchers}

class InterpreterTests extends FreeSpec with Matchers {
  "Print" - {
    "a string" in {
      val prog = Program(Seq(
        Print("pi=")
      ))
      val result = new Interpreter().run(prog)

      result.error shouldBe None
      result.output.mkString shouldBe "pi="
    }
    "multiple strings" in {
      val prog = Program(Seq(Print("Hello,"), Print(" World"), Print("!")))
      val result = new Interpreter().run(prog)

      result.output.mkString shouldBe "Hello, World!"
    }
  }
  "Output" - {
    "an integer" in {
      val prog = Program(Seq(
        Out(Literal(10))
      ))
      val result = new Interpreter().run(prog)

      result.output.mkString shouldBe "10"
    }
    "an real" in {
      val prog = Program(Seq(
        Out(Literal(1.0))
      ))
      val result = new Interpreter().run(prog)

      result.output.mkString shouldBe "1.0"
    }
    "an integer in round brackets" in {
      val prog = Program(Seq(
        Out(Brackets(Literal(42)))
      ))
      val result = new Interpreter().run(prog)

      result.output.mkString shouldBe "42"
    }
    "a value of arithmetic expression" in {
      val prog = Program(Seq(
        Out(
          Div(
            Pow(Brackets(Literal(-1)), Literal(10)),
            Brackets(
              Plus(Mul(Literal(2), Literal(10)), Literal(1))
            )
          )
        )
      ))
      val result = new Interpreter().run(prog)

      result.output.mkString shouldBe "0.047619047619047616"
    }
  }
  "Define variables" - {
    " and output its value" in {
      val prog = Program(Seq(
        VarDef("n", Literal(500)),
        Out(Id("n"))
      ))
      val result = new Interpreter().run(prog)

      result.output.mkString shouldBe "500"
    }
  }
}
