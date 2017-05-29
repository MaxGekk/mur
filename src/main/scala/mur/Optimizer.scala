package mur

object Optimizer {
  def optExpr(expr: Expr): Expr = expr match {
    case Brackets(e) => e
    case plus: Plus =>
      plus.copy(left = optExpr(plus.left), right = optExpr(plus.right))
    case minus: Minus =>
      minus.copy(left = optExpr(minus.left), right = optExpr(minus.right))
    case mul: Mul =>
      mul.copy(left = optExpr(mul.left), right = optExpr(mul.right))
    case div: Div =>
      div.copy(left = optExpr(div.left), right = optExpr(div.right))
    case reduce @ ReduceSeq(_, Literal(0 | 0.0D), Id(a), Id(b), Plus(Id(x), Id(y)))
      if (a == x && b == y) || (a == y && b == x) => SumSeq(reduce.seq)
    case other => other
  }

  def optStmt(stmt: Stmt): Stmt = stmt match {
    case out: Out => out.copy(expr = optExpr(out.expr))
    case vardef: VarDef => vardef.copy(expr = optExpr(vardef.expr))
    case print => print
  }

  def apply(prog: Program): Program = {
    prog.copy(stmts = prog.stmts.map(optStmt(_)))
  }
}
