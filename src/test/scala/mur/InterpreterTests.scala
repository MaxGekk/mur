package mur

import org.scalatest.{FreeSpec, Matchers}

class InterpreterTests extends FreeSpec with Matchers {
  "Print" - {
    "a string" in {
      val prog = Program(Seq(
        Print("pi=")
      ))
      val result = Interpreter.run(prog)

      result.right.get.mkString shouldBe "pi="
    }
    "multiple strings" in {
      val prog = Program(Seq(Print("Hello,"), Print(" World"), Print("!")))
      val result = Interpreter.run(prog)

      result.right.get.mkString shouldBe "Hello, World!"
    }
  }
  "Output" - {
    "an integer" in {
      val prog = Program(Seq(
        Out(Literal(10))
      ))
      val result = Interpreter.run(prog)

      result.right.get.mkString shouldBe "10"
    }
    "an real" in {
      val prog = Program(Seq(
        Out(Literal(1.0))
      ))
      val result = Interpreter.run(prog)

      result.right.get.mkString shouldBe "1.0"
    }
    "an integer in round brackets" in {
      val prog = Program(Seq(
        Out(Brackets(Literal(42)))
      ))
      val result = Interpreter.run(prog)

      result.right.get.mkString shouldBe "42"
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
      val result = Interpreter.run(prog)

      result.right.get.mkString shouldBe "0.047619047619047616"
    }
    "invalid expression" in {
      val prog = Program(Seq(
        Out(Literal('X'))
      ))
      val result = Interpreter.run(prog)

      result.isLeft shouldBe true
    }
  }
  "Define variables" - {
    " and output its value" in {
      val prog = Program(Seq(
        VarDef("n", Literal(500)),
        Out(Id("n"))
      ))
      val result = Interpreter.run(prog)

      result.right.get.mkString shouldBe "500"
    }
    "invalid expression" in {
      val prog = Program(Seq(
        VarDef("n", Plus(Literal(0), Sequence(Literal(1), Literal(2))))
      ))
      val result = Interpreter.run(prog)

      result.isLeft shouldBe true
    }
  }
}
