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
  def ::(seq: SingleValue): SeqValue
  def ++(seq: SeqValue): SeqValue
}
case class NumSeq(seq: List[Int]) extends SeqValue {
  def ::(single: SingleValue): SeqValue = single match {
    case n: Num => this.copy(n.value :: seq)
    case r: Real => RealSeq(r.value :: seq.map(_.toDouble))
  }
  def ++(other: SeqValue): SeqValue = other match {
    case ns: NumSeq => this.copy(seq ++ ns.seq)
    case rs: RealSeq => RealSeq(seq.map(_.toDouble) ++ rs.seq)
  }
}
case class RealSeq(seq: List[Double]) extends SeqValue {
  def ::(single: SingleValue): SeqValue = single match {
    case n: Num => this.copy(n.value.toDouble :: seq)
    case r: Real => this.copy(r.value :: seq)
  }
  def ++(other: SeqValue): SeqValue = other match {
    case ns: NumSeq => this.copy(seq ++ ns.seq.map(_.toDouble))
    case rs: RealSeq => RealSeq(seq ++ rs.seq)
  }
}

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

  def append(seq: ExprValue, elem: ExprValue): ExprValue = (seq, elem) match {
    case (s: SeqValue, el: SingleValue) => el :: s
    case (s1: SeqValue, s2: SeqValue) => s1 ++ s2
    case (_, _) => throw new NotImplementedError(s"seq = $seq (${seq.getClass.getName}) elem = $elem (${elem.getClass.getName})")
  }

  def single(x: AnyVal): ExprValue = {
    if (x.isInstanceOf[Double])
      Real(x.asInstanceOf[Double])
    else
      Num(x.asInstanceOf[Int])
  }
}


