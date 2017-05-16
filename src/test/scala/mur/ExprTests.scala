package mur

import org.scalatest.{FreeSpec, Matchers}

class ExprTests extends FreeSpec with Matchers {
  "Calculate the arithmetic expression" - {
    "plus mixed types" in {
      val expr = Plus(Plus(Literal(1.0), Literal(2)), Plus(Literal(1), Literal(2.0)))
      val result = Expr.calc(expr, Context())

      result.error shouldBe None
      result.value shouldBe Some(Real(6.0))
    }
    "minus mixed types" in {
      val expr = Minus(Minus(Literal(3.0), Literal(1)), Minus(Literal(2), Literal(1.0)))
      val result = Expr.calc(expr, Context())

      result.error shouldBe None
      result.value shouldBe Some(Real(1.0))
    }
    "mul mixed types" in {
      val expr = Mul(Mul(Literal(3.0), Literal(1)), Mul(Literal(2), Literal(1.0)))
      val result = Expr.calc(expr, Context())

      result.error shouldBe None
      result.value shouldBe Some(Real(6.0))
    }
    "div mixed types" in {
      val expr = Div(Div(Literal(4.0), Literal(1)), Div(Literal(2), Literal(1.0)))
      val result = Expr.calc(expr, Context())

      result.error shouldBe None
      result.value shouldBe Some(Real(2.0))
    }
    "pow mixed types" in {
      val expr = Pow(Pow(Literal(3.0), Literal(2)), Pow(Literal(2), Literal(2.0)))
      val result = Expr.calc(expr, Context())

      result.error shouldBe None
      result.value shouldBe Some(Real(6561.0))
    }
    "minus of 2 ints" in {
      val expr = Minus(Literal(10), Literal(9))
      val result = Expr.calc(expr, Context())

      result.error shouldBe None
      result.value shouldBe Some(Num(1))
    }
  }
  "Calculate the complex expression " - {
    "reduce a sequence" in {
      val expr = ReduceSeq(
        Sequence(Literal(1), Literal(10)),
        Literal(1),
        Id("x"), Id("y"),
        Mul(Id("x"), Id("y"))
      )
      val result = Expr.calc(expr, Context())

      result.error shouldBe None
      result.value shouldBe Some(Num(1 to 10 reduce(_ * _)))
    }
  }
  "Calculate the expression with " - {
    "wrong literal" in {
      val expr = Literal('a')
      val result = Expr.calc(expr, Context())

      result.error shouldBe Some("Invalid literal type (java.lang.Character)")
      result.value shouldBe None
    }
    "undefined identifier" in {
      val expr = Id("X")
      val result = Expr.calc(expr, Context())

      result.error shouldBe Some("Identifier X is not defined")
      result.value shouldBe None
    }
    "wrong sequence" in {
      val expr = Sequence(Literal(10), Literal(0))
      val result = Expr.calc(expr, Context())

      result.error shouldBe Some("Wrong params of the sequence: 10..0")
      result.value shouldBe None
    }
    "wrong plus operands - left" in {
      val expr = Plus(Sequence(Literal(2), Literal(3)), Literal(2.0))
      val result = Expr.calc(expr, Context())

      result.error shouldBe Some("Wrong left operand of '+': mur.NumSeq")
      result.value shouldBe None
    }
    "wrong plus operands - right" in {
      val expr = Plus(Literal(2.0), Sequence(Literal(2), Literal(3)))
      val result = Expr.calc(expr, Context())

      result.error shouldBe Some("Wrong right operand of '+': mur.NumSeq")
      result.value shouldBe None
    }
    "wrong minus operands - left" in {
      val expr = Minus(Sequence(Literal(2), Literal(3)), Literal(2.0))
      val result = Expr.calc(expr, Context())

      result.error shouldBe Some("Wrong left operand of '+': mur.NumSeq")
      result.value shouldBe None
    }
    "wrong minus operands - right" in {
      val expr = Minus(Literal(2.0), Sequence(Literal(2), Literal(3)))
      val result = Expr.calc(expr, Context())

      result.error shouldBe Some("Wrong right operand of '+': mur.NumSeq")
      result.value shouldBe None
    }
    "wrong mul operands - left" in {
      val expr = Mul(Sequence(Literal(2), Literal(3)), Literal(2.0))
      val result = Expr.calc(expr, Context())

      result.error shouldBe Some("Wrong left operand of '+': mur.NumSeq")
      result.value shouldBe None
    }
    "wrong mul operands - right" in {
      val expr = Mul(Literal(2.0), Sequence(Literal(2), Literal(3)))
      val result = Expr.calc(expr, Context())

      result.error shouldBe Some("Wrong right operand of '+': mur.NumSeq")
      result.value shouldBe None
    }
    "wrong div operands - left" in {
      val expr = Div(Sequence(Literal(2), Literal(3)), Literal(2.0))
      val result = Expr.calc(expr, Context())

      result.error shouldBe Some("Wrong left operand of '+': mur.NumSeq")
      result.value shouldBe None
    }
    "wrong div operands - right" in {
      val expr = Div(Literal(2.0), Sequence(Literal(2), Literal(3)))
      val result = Expr.calc(expr, Context())

      result.error shouldBe Some("Wrong right operand of '+': mur.NumSeq")
      result.value shouldBe None
    }
    "div zero - int" in {
      val expr = Div(Literal(2.0), Literal(0))
      val result = Expr.calc(expr, Context())

      result.error shouldBe Some("Division by zero")
      result.value shouldBe None
    }
    "div zero - real" in {
      val expr = Div(Literal(2.0), Literal(0.0))
      val result = Expr.calc(expr, Context())

      result.error shouldBe Some("Division by zero")
      result.value shouldBe None
    }
    "wrong pow operands - left" in {
      val expr = Pow(Sequence(Literal(2), Literal(3)), Literal(2.0))
      val result = Expr.calc(expr, Context())

      result.error shouldBe Some("Wrong left operand of '+': mur.NumSeq")
      result.value shouldBe None
    }
    "wrong pow operands - right" in {
      val expr = Pow(Literal(2.0), Sequence(Literal(2), Literal(3)))
      val result = Expr.calc(expr, Context())

      result.error shouldBe Some("Wrong right operand of '+': mur.NumSeq")
      result.value shouldBe None
    }
  }
}
