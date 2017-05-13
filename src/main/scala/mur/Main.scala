package mur

object Main {
  val text =
    """
      |var n = 500
      |var sequence = map({0, n}, i -> (-1)^i / (2 * i + 1))
      |var pi = 4 * reduce(sequence, 0, x y -> x + y)
      |print "pi = "
      |out pi
    """.stripMargin
  val prog = Program(Seq(
    VarDef("n", Literal(500)),
    VarDef("sequence",
      MapSeq(
        Sequence(Literal(0), Id("n")),
        Id("i"),
        Div(
          Pow(Literal(-1), Id("i")),
          Plus(Mul(Literal(2), Id("i")), Literal(1))
        )
      )
    ),
    VarDef("pi",
      Mul(Literal(4),
        ReduceSeq(
          Id("sequence"), Literal(0), Id("x"), Id("y"),
          Plus(Id("x"), Id("y"))
        )
      )
    ),
    Print("pi = "),
    Out(Id("pi"))
  ))
  def main(args: Array[String]): Unit = {
    val result = new Interpreter().run(prog)

    println(result.output.mkString)
  }
}
