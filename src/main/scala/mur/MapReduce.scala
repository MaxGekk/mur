package mur

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

/** Calculating results of the map and reduce operations */
object MapReduce {
  val CHUNK_SIZE = 8192

  def calc(op: MapSeq, ctx: Context): ExprResult = {
    def map(vals: List[AnyVal]): ExprResult = {
      val sliced = slice(vals, ctx)
      val futures = sliced.map { case (chunk, context) =>
        Future {
          val res = chunk.map { elem =>
            context.ids.put(op.x.name, toNumValue(elem))
            Expr.calc(op.expr, context)
          }
          union(res)
        }
      }
      val res = Await.result(Future.sequence(futures.toSeq.reverse), Duration.Inf)
      union(res)
    }
    // Materialisation of the first parameter - a sequence
    val range = Expr.calc(op.seq, ctx)
    range match {
      // Extract sequence of integers or doubles
      case ExprResult(Some(NumSeq(seq)), _) => map(seq)
      case ExprResult(Some(RealSeq(seq)), _) => map(seq)
      case ExprResult(Some(unknown), _) =>
        ExprResult(None, Some(s"map works over sequences only"))
      case error => error
    }
  }

  // Reduce a sequence: reduce(seq, init, x y -> x + y)
  def calc(op: ReduceSeq, ctx: Context): ExprResult = {
    val init = Expr.calc(op.init, ctx)

    def reduce(vals: List[AnyVal]): ExprResult = {
      def applyLambda(x: AnyVal, y: AnyVal, context: Context): ExprResult = {
        context.ids.put(op.x.name, toNumValue(x))
        context.ids.put(op.y.name, toNumValue(y))

        Expr.calc(op.expr, context)
      }
      // Materialise init (or neutral value) parameter
      val sliced = slice(vals, ctx)
      val futures = sliced.map { case (chunk, context) =>
        Future {
          // Iterate over the sequence staring from the init param and calculate
          // the result by applying the lambda function.
          chunk.foldLeft(init) {
            // Keep error and return it
            case (error @ ExprResult(None, _), _) => error

            case (ExprResult(Some(Num(n)), _), elem) => applyLambda(n, elem, context)
            case (ExprResult(Some(Real(n)), _), elem) => applyLambda(n, elem, context)
            case (ExprResult(Some(some), _), elem) =>
              ExprResult(None, Some(s"reduce produces wrong type: ${some.getClass.getName}"))
          }
        }
      }
      val res = Await.result(Future.sequence(futures), Duration.Inf)
      reduceRes(union(res.toIterable))
    }
    def reduceRes(results: ExprResult): ExprResult = results match {
      case ExprResult(Some(NumSeq(seq)), _) if seq.isEmpty => init
      case ExprResult(Some(NumSeq(seq)), _) if seq.length == 1 =>
        ExprResult(Some(Num(seq.head)))
      case ExprResult(Some(NumSeq(seq)), _) => reduce(seq)

      case ExprResult(Some(RealSeq(seq)), _) if seq.isEmpty => init
      case ExprResult(Some(RealSeq(seq)), _) if seq.length == 1 =>
        ExprResult(Some(Real(seq.head)))
      case ExprResult(Some(RealSeq(seq)), _) => reduce(seq)
      case ExprResult(Some(_), _) =>
        ExprResult(None, Some(s"reduce works over sequences only"))
      case error => error
    }

    // Materialise the sequence - first parameter
    val range = Expr.calc(op.seq, ctx)
    reduceRes(range)
  }

  def union(results: Iterable[ExprResult]): ExprResult = {
    results.foldRight(ExprResult(Some(NumSeq(List())))) {
      // Keep the first error and return it (ignore other values)
      case (error @ ExprResult(None, _), _) => error
      case (_, error @ ExprResult(None, _)) => error
      // Marge values of all results to one result with the sequence as its value
      case (ExprResult(Some(num), _), ExprResult(Some(s), _)) =>
        ExprResult(Some(ExprValue.append(s, num)))
    }
  }

  def slice(seq: List[AnyVal], ctx: Context) = {
    seq.sliding(CHUNK_SIZE, CHUNK_SIZE)
       .map((_, ctx.copy(ids = ctx.ids.clone())))
  }

  def toNumValue(x: AnyVal): ExprValue = x match {
    case i: Int => Num(i)
    case d: Double => Real(d)
  }
}
