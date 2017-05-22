package mur

import scala.collection.mutable

/** Error description */
case class Error(line: Int = -1, column: Int = -1, msg: String = "No error") {
  override def toString: String = {
    val suffix = if (line != -1) {
      if (column == -1) s" at line = ${line}" else s" at line=${line}, column=$column"
    } else ""
    msg ++ suffix
  }
}

/** Result of program interpretation
 *
  * @param output - strings printed by the out and print statements
  * @param error - first occurred error
  * */
case class Result(output: Seq[String] = Seq(), error: Option[Error] = None)

/** Interpreter settings
  * @param chunkSize - input for map/reduce is split by chunks.
  *                  The parameter controls maximum size of such chunks */
case class Settings(chunkSize: Int = 65536)

/** Context keeps state of interpretation
  * @param ids - mapping identifiers to its values
  * @param line - currently processing line. Starting from 0
  */
case class Context(
                    ids: mutable.Map[String, ExprValue] = mutable.Map(),
                    settings: Settings = Settings(),
                    var line: Int = -1
                  )

class Interpreter {
  def run(prog: Program): Result = {
    // Iterates over program statements one-by-one and execute them
    val init = (Context(), Result())
    val (_, result) = prog.stmts.zipWithIndex.foldLeft(init) { case (acc, (stmt, line)) =>
      val (ctx, res) = acc
      ctx.line = stmt.pos.line

      stmt match {
        // Print a string like : print "Hello, World!"
        case Print(str) => (ctx, res.copy(output = res.output :+ str))
        // Evaluate the expression, convert result to a string and print it
        case Out(expr) =>
          val exprResult = Expr.calc(expr, ctx)
          exprResult match {
            case ExprResult(Some(value), _) =>
              (ctx, res.copy(output = res.output :+ value.toString))
            case ExprResult(None, error) if res.error.isEmpty => (ctx, res.copy(error = error))
            case _ => (ctx, res)
          }
        // Define new variable (override old one), get its value eagerly
        // and keep it in the context
        case VarDef(id, expr) =>
          val exprResult = Expr.calc(expr, ctx)
          exprResult match {
            case ExprResult(Some(value), _) => (ctx.copy(ids = ctx.ids.updated(id, value)), res)
            case ExprResult(None, error) if res.error.isEmpty => (ctx, res.copy(error = error))
            case _ => (ctx, res)
          }
      }
    }

    result
  }
}
