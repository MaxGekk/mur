package mur

import scala.util.parsing.input.Positional

// Intermediate representation for expressions
sealed trait Expr extends Positional
case class Literal(value: AnyVal) extends Expr // 10, 3.14
case class Brackets(expr: Expr) extends Expr   // ()
case class Id(name: String) extends Expr // identifer - a string like abc123
case class Sequence(begin: Expr, end: Expr) extends Expr // {1, 100}

// Arithmetic operator like +,-,*,/,^. Example: 1 + 2
sealed trait Op extends Expr {
  def left: Expr   // left operand - 1 in the example
  def right: Expr  // right operand
}
case class Plus(left: Expr, right: Expr) extends Op  // 1 + 2
case class Minus(left: Expr, right: Expr) extends Op // 2.0 - 1
case class Mul(left: Expr, right: Expr) extends Op   // 3 * 4
case class Div(left: Expr, right: Expr) extends Op   // 6/3.3
case class Pow(left: Expr, right: Expr) extends Op   // 2 ^ 8

// Applying a lambda function to each element of a sequence:
// map(sequence, x -> x + 1)
case class MapSeq(seq: Expr, x: Id, expr: Expr) extends Expr
// Reducing a suquence by applying of a lambda function:
// reduce(sequence, init, x y -> x + y)
case class ReduceSeq(seq: Expr, init: Expr, x: Id, y: Id, expr: Expr) extends Expr

object Expr {
  // Result of calculation of an expression: value or error
  type Result = Either[Error, ExprValue]

  def calc(expr: Expr, context: Context): Result = {
    val ctx = context.copy(line = expr.pos.line, column = expr.pos.column)

    expr match {
      case Literal(d: Double) => Right(Real(d))
      case Literal(i: Int) => Right(Num(i))
      case Literal(v) => error(ctx, s"invalid literal type (${v.getClass.getName})")
      case Brackets(expr) => calc(expr, ctx)
      case Id(name) =>
        ctx.ids.get(name) match { // Look up the identifier in the ids map in context
          case None => error(ctx, s"identifier `$name` is not defined")
          case Some(value) => Right(value)
        }
      case Sequence(begin, end) =>
        // Materialise begin and end of the expression. Supported only Nums
        val (beginResult, endResult) = (calc(begin, ctx), calc(end, ctx))
        (beginResult, endResult) match {
          case (Right(Num(bv)), Right(Num(ev))) =>
            if (bv <= ev) // Supported only ascending sequence of numbers
              Right(NumSeq(bv to ev toList))
            else
              error(ctx, s"wrong params of the sequence: ${bv}..${ev}")
          case (error @ Left(_), _) => error
          case (_, error @ Left(_)) => error
          case (_, _) => error(ctx, s"wrong type of sequence begin or/and end")
        }
      case op: Op =>
        // Materialisation of left operand and after that right operand even if
        // the left operand is invalid.
        val (leftValue, rightValue) = (calc(op.left, ctx), calc(op.right, ctx))
        (leftValue, rightValue) match {
          case (error @ Left(_), _) => error
          case (_, error @ Left(_)) => error
          case (Right(lvalue), Right(rvalue)) =>
            op match {
              case _: Plus => ExprValue.plus(ctx, lvalue, rvalue)
              case _: Minus => ExprValue.minus(ctx, lvalue, rvalue)
              case _: Mul => ExprValue.mul(ctx, lvalue, rvalue)
              case _: Div => ExprValue.div(ctx, lvalue, rvalue)
              case _: Pow => ExprValue.pow(ctx, lvalue, rvalue)
            }
        }
      case map: MapSeq => MapReduce.calc(map, ctx)
      case reduce: ReduceSeq => MapReduce.calc(reduce, ctx)
    }
  }

  def error(ctx: Context, msg: String): Result = {
    Left(Error(msg = msg, line = ctx.line, column = ctx.column))
  }
}
