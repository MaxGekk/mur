package mur

// A value of a calculated expression. Support 2 kind of values:
// - single value - Num (integer) or Real (double)
// - sequence of single values - NumSeq and RealSeq
sealed trait ExprValue {
  def isSingle: Boolean
  def isSeq: Boolean = !isSingle
}
sealed trait SingleValue extends ExprValue {
  val value: AnyVal
  def isSingle: Boolean = true
  override def toString: String = value.toString
  def +(other: SingleValue): SingleValue
  def -(other: SingleValue): SingleValue
  def *(other: SingleValue): SingleValue
  def /(other: SingleValue): SingleValue
  def ^(other: SingleValue): SingleValue
  def isZero: Boolean
}

case class Num(value: Int) extends SingleValue {
  def +(other: SingleValue) = other match {
    case Num(i) => Num(value + i)
    case Real(r) => Real(value + r)
  }
  def -(other: SingleValue) = other match {
    case Num(i) => Num(value - i)
    case Real(r) => Real(value - r)
  }
  def *(other: SingleValue) = other match {
    case Num(i) => Num(value * i)
    case Real(r) => Real(value * r)
  }
  def /(other: SingleValue) = other match {
    case Num(i) => Real(value.toDouble / i)
    case Real(r) => Real(value / r)
  }
  def ^(other: SingleValue) = other match {
    case Num(i) => Num(scala.math.pow(value, i).toInt)
    case Real(r) => Real(scala.math.pow(value, r))
  }
  def isZero: Boolean = value == 0
}

case class Real(value: Double) extends SingleValue {
  def +(other: SingleValue) = other match {
    case Num(i) => Real(value + i)
    case Real(r) => Real(value + r)
  }
  def -(other: SingleValue) = other match {
    case Num(i) => Real(value - i)
    case Real(r) => Real(value - r)
  }
  def *(other: SingleValue) = other match {
    case Num(i) => Real(value * i)
    case Real(r) => Real(value * r)
  }
  def /(other: SingleValue) = other match {
    case Num(i) => Real(value.toDouble / i)
    case Real(r) => Real(value / r)
  }
  def ^(other: SingleValue) = other match {
    case Num(i) => Real(scala.math.pow(value, i))
    case Real(r) => Real(scala.math.pow(value, r))
  }
  def isZero: Boolean = value == 0D
}

sealed trait SeqValue extends ExprValue {
  val seq: List[AnyVal]
  def isSingle: Boolean = false
  override def toString: String = seq.mkString("{",",","}")
}
case class NumSeq(seq: List[Int]) extends SeqValue
case class RealSeq(seq: List[Double]) extends SeqValue

object ExprValue {
  import Expr.error

  def plus(ctx: Context, l: ExprValue, r: ExprValue): Expr.Result = (l, r) match {
    case (lv: SingleValue, rv: SingleValue) => Right(lv + rv)
    case (_: SingleValue, _) => error(ctx, s"wrong right operand of '+': ${r.getClass.getName}")
    case (_, _) => error(ctx, s"wrong left operand of '+': ${l.getClass.getName}")
  }

  def minus(ctx: Context, l: ExprValue, r: ExprValue): Expr.Result = (l, r) match {
    case (lv: SingleValue, rv: SingleValue) => Right(lv - rv)
    case (_: SingleValue, _) => error(ctx, s"wrong right operand of '-': ${r.getClass.getName}")
    case (_, _) => error(ctx, s"wrong left operand of '-': ${l.getClass.getName}")
  }

  def mul(ctx: Context, l: ExprValue, r: ExprValue): Expr.Result = (l, r) match {
    case (lv: SingleValue, rv: SingleValue) => Right(lv * rv)
    case (_: SingleValue, _) => error(ctx, s"wrong right operand of '*': ${r.getClass.getName}")
    case (_, _) => error(ctx, s"wrong left operand of '*': ${l.getClass.getName}")
  }

  def div(ctx: Context, l: ExprValue, r: ExprValue): Expr.Result = (l, r) match {
    case (_: SingleValue, rv: SingleValue) if rv.isZero => error(ctx, "division by zero")
    case (lv: SingleValue, rv: SingleValue) => Right(lv / rv)
    case (_: SingleValue, _) => error(ctx, s"wrong right operand of '/': ${r.getClass.getName}")
    case (_, _) => error(ctx, s"wrong left operand of '/': ${l.getClass.getName}")
  }

  def pow(ctx: Context, l: ExprValue, r: ExprValue): Expr.Result = (l, r) match {
    case (lv: SingleValue, rv: SingleValue) => Right(lv ^ rv)
    case (_: SingleValue, _) => error(ctx, s"wrong right operand of '^': ${r.getClass.getName}")
    case (_, _) => error(ctx, s"wrong left operand of '^': ${l.getClass.getName}")
  }

  def append(seq: ExprValue, elem: ExprValue): ExprValue = {
    (seq, elem) match {
      case (s @ NumSeq(sn: List[Int]), Num(n)) => s.copy(n :: sn)
      case (s @ NumSeq(sn: List[Int]), Real(n)) => RealSeq(n :: sn.map(_.toDouble))
      case (s @ RealSeq(sn: List[Double]), Num(n)) => s.copy(n.toDouble :: sn)
      case (s @ RealSeq(sn: List[Double]), Real(n)) => s.copy(n :: sn)
      case (s @ NumSeq(sn: List[Int]), RealSeq(sr: List[Double])) =>
        RealSeq(sn.map(_.toDouble) ++ sr)
      case (s @ RealSeq(sr1: List[Double]), RealSeq(sr2: List[Double])) =>
        s.copy(seq = sr1 ++ sr2)
      case (s @ NumSeq(sn1: List[Int]), NumSeq(sn2: List[Int])) =>
        s.copy(seq = sn1 ++ sn2)
      case (_, _) =>
        throw new NotImplementedError(s"seq = $seq (${seq.getClass.getName}) elem = $elem (${elem.getClass.getName})")
    }
  }

  def single(x: AnyVal): ExprValue = {
    if (x.isInstanceOf[Double])
      Real(x.asInstanceOf[Double])
    else
      Num(x.asInstanceOf[Int])
  }
}


