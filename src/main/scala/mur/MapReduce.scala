package mur

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import Expr.error
import ExprValue.single
import scala.collection.mutable

/** Calculating results of the map and reduce operations */
object MapReduce {
  implicit val ec = Worker.executionContext

  def calc(op: MapSeq, ctx: Context): Expr.Result = {
    def mapValues(vals: List[AnyVal]): Expr.Result = {
      val sliced = slice(vals, ctx)
      val futures = sliced.map { case (chunk, context) =>
        Future {
          val res = chunk.map { elem =>
            context.ids.put(op.x.name, single(elem))
            Expr.calc(op.expr, context)
          }
          union(res)
        }
      }
      val result = Future.sequence(futures.toSeq.reverse).map(union)
      Await.result(result, Duration.Inf)
    }
    // Materialisation of the first parameter - a sequence
    val range = Expr.calc(op.seq, ctx)
    range match {
      case Right(value) if !value.isSeq => error(ctx, s"map works over sequences only")
      // Extract sequence of integers or doubles
      case Right(value: SeqValue) => mapValues(value.seq)
      case error => error
    }
  }

  // Reduce a sequence: reduce(seq, init, x y -> x + y)
  def calc(op: ReduceSeq, ctx: Context): Expr.Result = {
    val init = Expr.calc(op.init, ctx)

    def reduceValues(vals: List[AnyVal]): Expr.Result = {
      def applyLambda(x: AnyVal, y: AnyVal, context: Context) = {
        context.ids.put(op.x.name, single(x))
        context.ids.put(op.y.name, single(y))

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
              error(ctx, s"lambda produces wrong type")
            case (Right(single: SingleValue), elem) => applyLambda(single.value, elem, context)
          }
        }
      }
      val merged = Future.sequence(futures.toIterable).map(union)
      val res = Await.result(merged, Duration.Inf)
      reduceResults(res)
    }
    def reduceResults(results: Expr.Result) = results match {
      case Right(value) if !value.isSeq => error(ctx, s"reduce works over sequences only")
      case Right(s: SeqValue) if s.seq.isEmpty => init
      case Right(s: SeqValue) if s.seq.length == 1 => Right(single(s.seq.head))
      case Right(s: SeqValue) => reduceValues(s.seq)
      case error => error
    }

    // Materialise the sequence - first parameter
    val range = Expr.calc(op.seq, ctx)
    reduceResults(range)
  }

  def calc(sum: SumSeq, ctx: Context): Expr.Result = {
    def sumValues[@specialized(Int, Double) T : Numeric](vals: Array[T]): T = {
      val sliced = vals.sliding(ctx.settings.chunkSize, ctx.settings.chunkSize)
      val futures = sliced.map { chunk => Future { chunk.sum } }
      val merged = Future.sequence(futures.toIterable).map(_.sum)
      Await.result(merged, Duration.Inf)
    }
    Expr.calc(sum.seq, ctx) match {
      case Right(NumSeq(s)) => Right(Num(sumValues(s.toArray)))
      case Right(RealSeq(s)) => Right(Real(sumValues(s.toArray)))
      case error => error
    }
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
       .map((_, ctx.copy(ids = mutable.Map())))
  }
}
