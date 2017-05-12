package mur

sealed trait Stmt
case class Print(str: String) extends Stmt
case class Out(expr: Expr) extends Stmt
case class VarDef(identifer: String, expr: Expr) extends Stmt

case class Program(stmts: Seq[Stmt])
