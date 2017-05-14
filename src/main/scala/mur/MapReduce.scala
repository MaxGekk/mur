package mur

import scala.collection.parallel.ParSeq

/** Calculating results of the map and reduce operations */
object MapReduce {
  def calc(map: MapSeq, ctx: Context): ExprResult = {
    def mapSeq(seq: ParSeq[AnyVal]): ExprResult = {
      // Apply lambda expression (the last parameter) to each element of the sequence
      val res = seq.map(elem =>
        // Make a copy of map.expr, find the lambda parameter (map.x)
        // in map.expr and replace it by an element of the sequence
        Expr.calc(Expr.transform(map.expr, map.x, Literal(elem)), ctx)
      )
      // Convert the sequence of expression results to one result by
      // collecting all values or getting the first error
      res.foldLeft(ExprResult(Some(NumSeq(ParSeq())))) {
        // Keep the first error and return it (ignore other values)
        case (error @ ExprResult(None, _), _) => error
        case (_, error @ ExprResult(None, _)) => error
        // Marge values of all results to one result with the sequence as its value
        case (ExprResult(Some(seq), _), ExprResult(Some(num), _)) =>
          ExprResult(Some(ExprValue.append(seq, num)))
      }
    }
    // Materialisation of the first parameter - a sequence
    val range = Expr.calc(map.seq, ctx)
    range match {
      // Extract sequence of integers or doubles
      case ExprResult(Some(NumSeq(seq)), _) => mapSeq(seq)
      case ExprResult(Some(RealSeq(seq)), _) => mapSeq(seq)
      case ExprResult(Some(unknown), _) =>
        ExprResult(None, Some(s"map works over sequences only"))
      case error => error
    }
  }
  // Reduce a sequence: reduce(seq, init, x y -> x + y)
  def calc(reduce: ReduceSeq, ctx: Context): ExprResult = {
    def reduceSeq(seq: ParSeq[AnyVal]): ExprResult = {
      // Replace lambda parameters by sequence values. It makes a copy of
      // reduce.expr, finds the first reduce.x and second reduce.y lambda parameters,
      // and replaced it by the provided values
      def reduceOp(x: AnyVal, y: AnyVal): ExprResult = {
        val ex = Expr.transform(reduce.expr, reduce.x, Literal(x))
        val ey = Expr.transform(ex, reduce.y, Literal(y))

        Expr.calc(ey, ctx) // Just calculate the lambda: x y -> x + y
      }
      // Materialise init (or neutral value) parameter
      val init = Expr.calc(reduce.init, ctx)
      // Iterate over the sequence staring from the init param and calculate
      // the result by applying the lambda function.
      seq.foldLeft(init) {
        // Keep error and return it
        case (error @ ExprResult(None, _), _) => error

        case (ExprResult(Some(Num(n)), _), elem) => reduceOp(n, elem)
        case (ExprResult(Some(Real(n)), _), elem) => reduceOp(n, elem)
        case (ExprResult(Some(some), _), elem) =>
          ExprResult(None, Some(s"reduce produces wrong type: ${some.getClass.getName}"))
      }
    }
    // Materialise the sequence - first parameter
    val range = Expr.calc(reduce.seq, ctx)
    range match {
      case ExprResult(Some(NumSeq(seq)), _) => reduceSeq(seq)
      case ExprResult(Some(RealSeq(seq)), _) => reduceSeq(seq)
      case ExprResult(Some(_), _) =>
        ExprResult(None, Some(s"reduce works over sequences only"))
      case error => error
    }
  }
}
