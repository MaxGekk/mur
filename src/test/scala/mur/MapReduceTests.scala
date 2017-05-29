package mur

import org.scalatest.{FreeSpec, Matchers}

class MapReduceTests extends FreeSpec with Matchers {
  "Mapping of sequence of" - {
    " ints" in {
      val input = Sequence(Literal(0), Literal(3))
      val expr = MapSeq(input, Id("x"), Mul(Id("x"), Literal(2)))
      val result = MapReduce.calc(expr, Context())

      result shouldBe Right(NumSeq(List(0, 2, 4, 6)))
    }
    " ints to produce another sequence of reals" in {
      val input = Sequence(Literal(0), Literal(3))
      val expr = MapSeq(input, Id("i"),
        Div(
          Pow(Literal(-1), Id("i")),
          Plus(Mul(Literal(2), Id("i")), Literal(1))
        )
      )
      val result = MapReduce.calc(expr, Context())
      val expected: List[Double] = (0 to 3).map (i => Math.pow(-1, i) / (2 * i + 1)).toList

      result shouldBe Right(RealSeq(expected))
    }
    " doubles" in {
      val input = Sequence(Literal(1), Literal(2))
      val inputSeq = MapSeq(input, Id("x"), Div(Id("x"), Literal(3.14)))
      val expr = MapSeq(inputSeq, Id("x"), Mul(Id("x"), Literal(0.5)))

      val result = MapReduce.calc(expr, Context())

      result shouldBe Right(RealSeq(List(0.1592356687898089, 0.3184713375796178)))
    }
    " single int. An error message should be returned." in {
      val input = Literal(1)
      val expr = MapSeq(input, Id("x"), Mul(Id("x"), Literal(0.5)))

      val result = MapReduce.calc(expr, Context())

      result.left.get.msg shouldBe "map works over sequences only"
    }
  }
  "Reducing of sequence of" - {
    " ints" in {
      val input = Sequence(Literal(0), Literal(3))
      val expr = ReduceSeq(input, Literal(0), Id("x"), Id("y"), Plus(Id("x"), Id("y")))
      val result = MapReduce.calc(expr, Context())

      result shouldBe Right(Num(0 + 1 + 2 + 3))
    }
    " doubles" in {
      val input = Sequence(Literal(0), Literal(1))
      val inputSeq = MapSeq(input, Id("x"), Plus(Id("x"), Literal(3.14)))
      val expr = ReduceSeq(inputSeq, Literal(1.0), Id("x"), Id("y"), Mul(Id("x"), Id("y")))
      val result = MapReduce.calc(expr, Context())

      result shouldBe Right(Real(1.0*3.14*(1.0 + 3.14)))
    }
    " not sequence" in {
      val input = Literal(0)
      val expr = ReduceSeq(input, Literal(1.0), Id("x"), Id("y"), Mul(Id("x"), Id("y")))
      val result = MapReduce.calc(expr, Context())

      result.left.get.msg shouldBe "reduce works over sequences only"
    }
    " sequence but produce not numbers" in {
      val input = Sequence(Literal(0), Literal(1))
      val expr = ReduceSeq(input, Literal(1.0), Id("x"), Id("y"), Sequence(Literal(0), Literal(1)))
      val result = MapReduce.calc(expr, Context())

      result.left.get.msg shouldBe "lambda produces wrong type"
    }
  }
}