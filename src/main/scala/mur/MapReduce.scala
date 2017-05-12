package mur

object MapReduce {
  def calc(map: MapSeq, ctx: Context): ExprResult = {
//    val range = Expr.calc(map.seq, ctx)
//    range match {
//      case ExprResult(Some(NumRange(r)), _) =>
//        r.map(i => Expr.calc(Expr.transform(map.expr, map.x, Literal(i)), ctx))
//    }
    ???
  }
  def calc(map: ReduceSeq, ctx: Context): ExprResult = ???
}
