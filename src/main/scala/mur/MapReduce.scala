package mur

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import Expr.error

/** Calculating results of the map and reduce operations */
object MapReduce {
  def calc(op: MapSeq, ctx: Context): Expr.Result = {
    def map(vals: List[AnyVal]): Expr.Result = {
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
      case Right(value) if !value.isSeq =>
        error(ctx, s"map works over sequences only")
      // Extract sequence of integers or doubles
      case Right(s: SeqValue) => map(s.seq)
      case error => error
    }
  }

  // Reduce a sequence: reduce(seq, init, x y -> x + y)
  def calc(op: ReduceSeq, ctx: Context): Expr.Result = {
    val init = Expr.calc(op.init, ctx)

    def reduce(vals: List[AnyVal]): Expr.Result = {
      def applyLambda(x: AnyVal, y: AnyVal, context: Context) = {
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
            case (error @ Left(_), _) => error
            case (Right(value), _) if !value.isSingle =>
              error(ctx, s"reduce produces wrong type: ${value.getClass.getName}")
            case (Right(single: SingleValue), elem) =>
              applyLambda(single.value, elem, context)
          }
        }
      }
      val res = Await.result(Future.sequence(futures), Duration.Inf)
      reduceRes(union(res.toIterable))
    }
    def reduceRes(results: Expr.Result) = results match {
      case Right(value) if !value.isSeq =>
        error(ctx, s"reduce works over sequences only")

      case Right(s: SeqValue) if s.seq.isEmpty => init
      case Right(s: SeqValue) if s.seq.length == 1 => Right(toNumValue(s.seq.head))
      case Right(s: SeqValue) => reduce(s.seq)

      case error => error
    }

    // Materialise the sequence - first parameter
    val range = Expr.calc(op.seq, ctx)
    reduceRes(range)
  }

  def union(results: Iterable[Expr.Result]): Expr.Result = {
    val initial: Expr.Result = Right(NumSeq(List()))
    results.foldRight(initial) {
      // Keep the first error and return it (ignore other values)
      case (error @ Left(_), _) => error
      case (_, error @ Left(_)) => error
      // Marge values of all results to one result with the sequence as its value
      case (Right(num), Right(s)) => Right(ExprValue.append(s, num))
    }
  }

  def slice(seq: List[AnyVal], ctx: Context) = {
    seq.sliding(ctx.settings.chunkSize, ctx.settings.chunkSize)
       .map((_, ctx.copy(ids = ctx.ids.clone())))
  }

  def toNumValue(x: AnyVal): ExprValue = x match {
    case i: Int => Num(i)
    case d: Double => Real(d)
  }
}
