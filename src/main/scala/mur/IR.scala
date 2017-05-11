package mur

sealed trait Result
case class Num(value: Int) extends Result
case class Real(value: Double) extends Result

trait Expr {
  def calc: Result
}

case class Id(name: String) extends Expr {
  def calc = ??? // Look up the symbol table
}

sealed trait Number extends Expr
case class IntNumber(n: Int) extends Number {
  def calc: Result = Num(n)
}
case class RealNumber(n: Double) extends Number {
  def calc: Result = Real(n)
}

sealed trait Op extends Expr {
  def left: Expr
  def right: Expr
}

case class Plus(left: Expr, right: Expr) extends Op {
  def calc: Result = (left.calc, right.calc) match {
    case (Num(l), Num(r)) => Num(l + r)
    case (Real(l), Num(r)) => Real(l + r)
    case (Num(l), Real(r)) => Real(l + r)
    case (Real(l), Real(r)) => Real(l + r)
  }
}
case class Minus(left: Expr, right: Expr) extends Op {
  def calc: Result = (left.calc, right.calc) match {
    case (Num(l), Num(r)) => Num(l - r)
    case (Real(l), Num(r)) => Real(l - r)
    case (Num(l), Real(r)) => Real(l - r)
    case (Real(l), Real(r)) => Real(l - r)
  }
}
case class Mul(left: Expr, right: Expr) extends Op {
  def calc: Result = (left.calc, right.calc) match {
    case (Num(l), Num(r)) => Num(l * r)
    case (Real(l), Num(r)) => Real(l * r)
    case (Num(l), Real(r)) => Real(l * r)
    case (Real(l), Real(r)) => Real(l * r)
  }
}
case class Div(left: Expr, right: Expr) extends Op {
  def calc: Result = (left.calc, right.calc) match {
    case (Num(l), Num(r)) => Num(l / r)
    case (Real(l), Num(r)) => Real(l / r)
    case (Num(l), Real(r)) => Real(l / r)
    case (Real(l), Real(r)) => Real(l / r)
  }
}
case class Pow(left: Expr, right: Expr) extends Op {
  import scala.math.pow
  def calc: Result = (left.calc, right.calc) match {
    case (Num(l), Num(r)) => Num(pow(l, r).intValue)
    case (Real(l), Num(r)) => Real(pow(l, r))
    case (Num(l), Real(r)) => Real(pow(l, r))
    case (Real(l), Real(r)) => Real(pow(l, r))
  }
}
