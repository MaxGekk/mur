package mur

import org.scalatest.{FreeSpec, Matchers}

import scala.collection.parallel.ParSeq

class MapReduceTests extends FreeSpec with Matchers {
  "Mapping of sequence of" - {
    " ints" in {
      val input = Sequence(Literal(0), Literal(3))
      val expr = MapSeq(input, Id("x"), Mul(Id("x"), Literal(2)))
      val result = MapReduce.calc(expr, Context())

      result shouldBe ExprResult(Some(NumSeq(ParSeq(0, 2, 4, 6))), None)
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
      val expected: ParSeq[Double] = (0 to 3).par.map (i => Math.pow(-1, i) / (2 * i + 1))

      result shouldBe ExprResult(Some(RealSeq(expected)), None)
    }
    " doubles" in {
      val input = Sequence(Literal(1), Literal(2))
      val inputSeq = MapSeq(input, Id("x"), Div(Id("x"), Literal(3.14)))
      val expr = MapSeq(inputSeq, Id("x"), Mul(Id("x"), Literal(0.5)))

      val result = MapReduce.calc(expr, Context())

      result shouldBe ExprResult(Some(RealSeq(ParSeq(0.1592356687898089, 0.3184713375796178))), None)
    }
    " single int. An error message should be returned." in {
      val input = Literal(1)
      val expr = MapSeq(input, Id("x"), Mul(Id("x"), Literal(0.5)))

      val result = MapReduce.calc(expr, Context())

      result shouldBe ExprResult(None, Some("map works over sequences only"))
    }
  }
  "Reducing of sequence of" - {
    " ints" in {
      val input = Sequence(Literal(0), Literal(3))
      val expr = ReduceSeq(input, Literal(0), Id("x"), Id("y"), Plus(Id("x"), Id("y")))
      val result = MapReduce.calc(expr, Context())

      result shouldBe ExprResult(Some(Num(0 + 1 + 2 + 3)), None)
    }
    " doubles" in {
      val input = Sequence(Literal(0), Literal(1))
      val inputSeq = MapSeq(input, Id("x"), Plus(Id("x"), Literal(3.14)))
      val expr = ReduceSeq(inputSeq, Literal(1.0), Id("x"), Id("y"), Mul(Id("x"), Id("y")))
      val result = MapReduce.calc(expr, Context())

      result shouldBe ExprResult(Some(Real(1.0*3.14*(1.0 + 3.14))), None)
    }
    " not sequence" in {
      val input = Literal(0)
      val expr = ReduceSeq(input, Literal(1.0), Id("x"), Id("y"), Mul(Id("x"), Id("y")))
      val result = MapReduce.calc(expr, Context())

      result shouldBe ExprResult(None, Some("reduce works over sequences only"))
    }
    " sequence but produce not numbers" in {
      val input = Sequence(Literal(0), Literal(1))
      val expr = ReduceSeq(input, Literal(1.0), Id("x"), Id("y"), Sequence(Literal(0), Literal(1)))
      val result = MapReduce.calc(expr, Context())

      result shouldBe ExprResult(None, Some("reduce produces wrong type: mur.NumSeq"))
    }
  }
}