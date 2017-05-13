package mur

import org.scalatest.{FreeSpec, Matchers}

class ExprTests  extends FreeSpec with Matchers {
  "Mapping of sequence of" - {
    " ints" in {
      val input = Sequence(Literal(0), Literal(3))
      val expr = MapSeq(input, Id("x"), Mul(Id("x"), Literal(2)))
      val result = MapReduce.calc(expr, Context())

      result shouldBe ExprResult(Some(NumSeq(Seq(0, 2, 4, 6))), None)
    }
    " reals" in {
      val input = Sequence(Literal(0), Literal(3))
      val expr = MapSeq(input, Id("i"),
        Div(
          Pow(Literal(-1), Id("i")),
          Plus(Mul(Literal(2), Id("i")), Literal(1))
        )
      )
      val result = MapReduce.calc(expr, Context())
      val expected: Seq[Double] = 0 to 3 map (i => Math.pow(-1, i)/(2*i + 1))

      result shouldBe ExprResult(Some(RealSeq(expected)), None)
    }
  }
}
