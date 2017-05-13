package mur

object MapReduce {
  def calc(map: MapSeq, ctx: Context): ExprResult = {
    def mapSeq(seq: Seq[AnyVal]): ExprResult = {
      val res = seq.map(elem =>
        Expr.calc(Expr.transform(map.expr, map.x, Literal(elem)), ctx)
      )
      res.foldLeft(ExprResult(Some(NumSeq(Seq())))) {
        // Keep error and return it
        case (error @ ExprResult(None, _), _) => error
        case (_, error @ ExprResult(None, _)) => error

        case (ExprResult(Some(seq), _), ExprResult(Some(num), _)) =>
          ExprResult(Some(ExprValue.append(seq, num)))
      }
    }
    val range = Expr.calc(map.seq, ctx)
    range match {
      case ExprResult(Some(NumSeq(seq)), _) => mapSeq(seq)
      case ExprResult(Some(RealSeq(seq)), _) => mapSeq(seq)
      case error => error
    }
  }
  def calc(map: ReduceSeq, ctx: Context): ExprResult = ???
}
