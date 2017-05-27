package mur

import scala.collection.mutable
import scala.util.parsing.input.Position

/** Error description */
case class Error(pos: Position, msg: String = "No error") {
  override def toString: String = s"at [${pos.line},${pos.column}]: $msg"
  def end(prog: Program): Option[Position] = {
    val set = mutable.Set[Position]()

    Program.positions(prog, set)
    set.filter(pos < _).toSeq.sortWith((x, y) => x < y).headOption
  }
}
