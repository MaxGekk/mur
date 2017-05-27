package mur

import scala.collection.mutable
import scala.util.parsing.input.{Position, Positional}

sealed trait Stmt extends Positional
case class Print(str: String) extends Stmt
case class Out(expr: Expr) extends Stmt
case class VarDef(identifier: String, expr: Expr) extends Stmt

case class Program(stmts: Seq[Stmt])

object Program {
  def positions(prog: Program, set: mutable.Set[Position]): Unit = {
    prog.stmts foreach {stmt =>
      set.add(stmt.pos)
      stmt match {
        case _: Print => ()
        case Out(expr) =>
          Expr.positions(expr, set)
        case VarDef(_, expr) =>
          Expr.positions(expr, set)
      }
    }
  }
}
