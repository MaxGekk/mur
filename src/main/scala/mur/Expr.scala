package mur

sealed trait Expr
case class Literal(value: AnyVal) extends Expr
case class Brackets(expr: Expr) extends Expr
case class Identifier(name: String) extends Expr

sealed trait Op extends Expr {
  def left: Expr
  def right: Expr
}
case class Plus(left: Expr, right: Expr) extends Op
case class Minus(left: Expr, right: Expr) extends Op
case class Mul(left: Expr, right: Expr) extends Op
case class Div(left: Expr, right: Expr) extends Op
case class Pow(left: Expr, right: Expr) extends Op

sealed trait ExprValue
case class Num(value: Int) extends ExprValue {
  override def toString: String = value.toString
}
case class Real(value: Double) extends ExprValue {
  override def toString: String = value.toString
}

case class ExprResult(value: Option[ExprValue], error: Option[String] = None)

object Expr {
  def calc(expr: Expr, ctx: Context): ExprResult = {
    expr match {
      case Literal(d: Double) => ExprResult(Some(Real(d)))
      case Literal(i: Int) => ExprResult(Some(Num(i)))
      case Literal(v) => ExprResult(None, Some(s"invalid literal type (${v.getClass.getName})"))
      case Brackets(expr) => calc(expr, ctx)
      case Identifier(name) =>
        ctx.ids.get(name) match {
          case None => ExprResult(None, Some(s"Identifier $name is not defined"))
          case value => ExprResult(value)
        }
      case op: Op =>
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
}