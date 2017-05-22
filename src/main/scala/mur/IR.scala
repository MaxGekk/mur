package mur

import scala.util.parsing.input.Positional

sealed trait Stmt extends Positional
case class Print(str: String) extends Stmt
case class Out(expr: Expr) extends Stmt
case class VarDef(identifer: String, expr: Expr) extends Stmt

case class Program(stmts: Seq[Stmt])
