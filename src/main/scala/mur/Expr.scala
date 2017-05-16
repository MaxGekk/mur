package mur

// Intermediate representation for expressions
sealed trait Expr
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

// A value of a calculated expression. Support 2 kind of values:
// - single value - Num (integer) or Real (double)
// - sequence of single values - NumSeq and RealSeq
sealed trait ExprValue
case class Num(value: Int) extends ExprValue {
  override def toString: String = value.toString
}
case class NumSeq(seq: Seq[Int]) extends ExprValue {
  override def toString: String = seq.mkString("{",",","}")
}
case class Real(value: Double) extends ExprValue {
  override def toString: String = value.toString
}
case class RealSeq(seq: Seq[Double]) extends ExprValue {
  override def toString: String = seq.mkString("{",",","}")
}
// Result of calculation of an expression: value or error
case class ExprResult(value: Option[ExprValue], error: Option[String] = None)

object Expr {
  def calc(expr: Expr, ctx: Context): ExprResult = {
    expr match {
      case Literal(d: Double) => ExprResult(Some(Real(d)))
      case Literal(i: Int) => ExprResult(Some(Num(i)))
      case Literal(v) => ExprResult(None, Some(s"Invalid literal type (${v.getClass.getName})"))
      case Brackets(expr) => calc(expr, ctx)
      case Id(name) =>
        ctx.ids.get(name) match { // Look up the identifier in the ids map in context
          case None => ExprResult(None, Some(s"Identifier $name is not defined"))
          case value => ExprResult(value)
        }
      case Sequence(begin, end) =>
        // Materialise begin and end of the expression. Supported only Nums
        val (beginResult, endResult) = (calc(begin, ctx), calc(end, ctx))
        (beginResult, endResult) match {
          case (ExprResult(Some(Num(bv)), None), ExprResult(Some(Num(ev)), None)) =>
            if (bv <= ev) // Supported only ascending sequence of numbers
              ExprResult(Some(NumSeq(bv to ev)), None)
            else
              ExprResult(None, Some(s"Wrong params of the sequence: ${bv}..${ev}"))
          case (error @ ExprResult(None, _), _) => error
          case (_, error @ ExprResult(None, _)) => error
        }
      case op: Op =>
        // Materialisation of left operand and after that right operand even if
        // the left operand is invalid.
        val (leftValue, rightValue) = (calc(op.left, ctx), calc(op.right, ctx))
        (leftValue, rightValue) match {
          case (lErr @ ExprResult(None, _), _) => lErr
          case (_, rErr @ ExprResult(None, _)) => rErr
          case (ExprResult(Some(lvalue), _), ExprResult(Some(rvalue), _)) =>
            op match {
              case _: Plus => ExprValue.plus(lvalue, rvalue)
              case _: Minus => ExprValue.minus(lvalue, rvalue)
              case _: Mul => ExprValue.mul(lvalue, rvalue)
              case _: Div => ExprValue.div(lvalue, rvalue)
              case _: Pow => ExprValue.pow(lvalue, rvalue)
            }
        }
      case map: MapSeq => MapReduce.calc(map, ctx)
      case reduce: ReduceSeq => MapReduce.calc(reduce, ctx)
    }
  }
}

object ExprValue {
  def plus(l: ExprValue, r: ExprValue): ExprResult = (l, r) match {
    case (Num(li), Num(ri)) => ExprResult(Some(Num(li + ri))) // Should we handle overflow ?
    case (Num(li), Real(rr)) => ExprResult(Some(Real(li + rr)))
    case (Real(lr), Num(ri)) => ExprResult(Some(Real(lr + ri)))
    case (Real(lr), Real(rr)) => ExprResult(Some(Real(lr + rr)))
    case (_: Num | _: Real, _) => ExprResult(None, Some(s"Wrong right operand of '+': ${r.getClass.getName}"))
    case (_, _) => ExprResult(None, Some(s"Wrong left operand of '+': ${l.getClass.getName}"))
  }
  def minus(l: ExprValue, r: ExprValue): ExprResult = (l, r) match {
    case (Num(li), Num(ri)) => ExprResult(Some(Num(li - ri))) // TODO: Handle overflow of integers
    case (Num(li), Real(rr)) => ExprResult(Some(Real(li - rr)))
    case (Real(lr), Num(ri)) => ExprResult(Some(Real(lr - ri)))
    case (Real(lr), Real(rr)) => ExprResult(Some(Real(lr - rr)))
    case (_: Num | _: Real, _) => ExprResult(None, Some(s"Wrong right operand of '+': ${r.getClass.getName}"))
    case (_, _) => ExprResult(None, Some(s"Wrong left operand of '+': ${l.getClass.getName}"))
  }
  def mul(l: ExprValue, r: ExprValue): ExprResult = (l, r) match {
    case (Num(li), Num(ri)) => ExprResult(Some(Num(li * ri))) // TODO: Handle overflow of integers
    case (Num(li), Real(rr)) => ExprResult(Some(Real(li * rr)))
    case (Real(lr), Num(ri)) => ExprResult(Some(Real(lr * ri)))
    case (Real(lr), Real(rr)) => ExprResult(Some(Real(lr * rr)))
    case (_: Num | _: Real, _) => ExprResult(None, Some(s"Wrong right operand of '+': ${r.getClass.getName}"))
    case (_, _) => ExprResult(None, Some(s"Wrong left operand of '+': ${l.getClass.getName}"))
  }
  def div(l: ExprValue, r: ExprValue): ExprResult = (l, r) match {
    case (_, Num(ri)) if ri == 0 => ExprResult(None, Some("Division by zero"))
    case (_, Real(rr)) if rr == 0.0D => ExprResult(None, Some("Division by zero"))
    case (Num(li), Num(ri)) => ExprResult(Some(Real(li.toDouble / ri)))
    case (Num(li), Real(rr)) => ExprResult(Some(Real(li / rr)))
    case (Real(lr), Num(ri)) => ExprResult(Some(Real(lr / ri)))
    case (Real(lr), Real(rr)) => ExprResult(Some(Real(lr / rr)))
    case (_: Num | _: Real, _) => ExprResult(None, Some(s"Wrong right operand of '+': ${r.getClass.getName}"))
    case (_, _) => ExprResult(None, Some(s"Wrong left operand of '+': ${l.getClass.getName}"))
  }
  def pow(l: ExprValue, r: ExprValue): ExprResult = (l, r) match {
    case (Num(li), Num(ri)) => ExprResult(Some(Num(scala.math.pow(li, ri).toInt))) // Should we handle overflow ?
    case (Num(li), Real(rr)) => ExprResult(Some(Real(scala.math.pow(li, rr))))
    case (Real(lr), Num(ri)) => ExprResult(Some(Real(scala.math.pow(lr, ri))))
    case (Real(lr), Real(rr)) => ExprResult(Some(Real(scala.math.pow(lr, rr))))
    case (_: Num | _: Real, _) => ExprResult(None, Some(s"Wrong right operand of '+': ${r.getClass.getName}"))
    case (_, _) => ExprResult(None, Some(s"Wrong left operand of '+': ${l.getClass.getName}"))
  }

  def append(seq: ExprValue, elem: ExprValue): ExprValue = {
    (seq, elem) match {
      case (s @ NumSeq(sn: Seq[Int]), Num(n)) => s.copy(sn :+ n)
      case (s @ NumSeq(sn: Seq[Int]), Real(n)) => RealSeq(sn.map(_.toDouble) :+ n)
      case (s @ RealSeq(sn: Seq[Double]), Num(n)) => s.copy(sn :+ n.toDouble)
      case (s @ RealSeq(sn: Seq[Double]), Real(n)) => s.copy(sn :+ n)
      case (_, _) => throw new NotImplementedError(s"seq = $seq elem = $elem")
    }
  }
}
