package mur

import com.typesafe.config.ConfigFactory

import scala.collection.mutable
import scala.util.parsing.input.{NoPosition, Position}

/** Interpreter settings
  * @param chunkSize - input for map/reduce is split by chunks.
  *                  The parameter controls maximum size of such chunks */
case class Settings(
                     chunkSize: Int = ConfigFactory.load.getInt("map-reduce.chunk-size")
                   )

/** Context keeps state of interpretation
  * @param ids - mapping identifiers to its values
  * @param settings - parameters for the interpreter
  * @param pos - position of currently processing expression.
  */
case class Context(
                    ids: mutable.Map[String, ExprValue] = mutable.Map(),
                    settings: Settings = Settings(),
                    pos: Position = NoPosition
                  )

object Interpreter {
  type Result = Either[Error, Seq[String]]

  /**
    * Interpretation of an intermediate representation of a progmam
    * @param prog - sequence of statements like print, out and var
    * @return either output of the out and print statements nor an error
    */
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
