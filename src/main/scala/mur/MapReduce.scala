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
      case ExprResult(Some(_), _) => ExprResult(None, Some(s"map works over sequences only: $range"))
      case error => error
    }
  }
  def calc(reduce: ReduceSeq, ctx: Context): ExprResult = {
    def reduceSeq(seq: Seq[AnyVal]): ExprResult = {
      def reduceOp(x: AnyVal, y: AnyVal): ExprResult = {
        val ex = Expr.transform(reduce.expr, reduce.x, Literal(x))
        val ey = Expr.transform(ex, reduce.y, Literal(y))

        Expr.calc(ey, ctx)
      }
      val init = Expr.calc(reduce.init, ctx)
      seq.foldLeft(init) {
        // Keep error and return it
        case (error @ ExprResult(None, _), _) => error

        case (ExprResult(Some(Num(n)), _), elem) => reduceOp(n, elem)
        case (ExprResult(Some(Real(n)), _), elem) => reduceOp(n, elem)
        case (ExprResult(Some(some), _), elem) =>
          ExprResult(None, Some(s"reduce produces wrong type: ${some.getClass.getName}"))
      }
    }
    val range = Expr.calc(reduce.seq, ctx)
    range match {
      case ExprResult(Some(NumSeq(seq)), _) => reduceSeq(seq)
      case ExprResult(Some(RealSeq(seq)), _) => reduceSeq(seq)
      case ExprResult(Some(_), _) => ExprResult(None, Some(s"reduce works over sequences only: $range"))
      case error => error
    }
  }
}
