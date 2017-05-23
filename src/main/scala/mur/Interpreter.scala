package mur

import scala.collection.mutable

/** Error description */
case class Error(line: Int, column: Int, msg: String = "No error") {
  override def toString: String = {
    val suffix = s" at line=${line}, column=$column"
    msg ++ suffix
  }
}

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
                    line: Int = 0,
                    column: Int = 0
                  )

object Interpreter {
  type Result = Either[Error, Seq[String]]

  def run(prog: Program): Result = {
    var ctx = Context()
    var result: Result = Right(Seq())

    for (stmt <- prog.stmts if result.isRight) {
      stmt match {
        // Print a string like : print "Hello, World!"
        case Print(str) =>
          result = Right(result.right.get :+ str)
        // Evaluate the expression, convert result to a string and print it
        case Out(expr) =>
          Expr.calc(expr, ctx) match {
            case Right(value) =>
              result = Right(result.right.get :+ value.toString)
            case Left(error) =>
              result = Left(error)
          }
        // Define new variable (override old one), get its value eagerly
        // and keep it in the context
        case VarDef(id, expr) =>
          Expr.calc(expr, ctx) match {
            case Right(value) =>
              ctx = ctx.copy(ids = ctx.ids.updated(id, value))
            case Left(error) =>
              result = Left(error)
          }
      }
    }

    result
  }
}
