package mur

/** Result of program interpretation
  * @param output - strings printed by the out and print statements
  * @param error - first occurred error
  * */
case class Result(output: Seq[String] = Seq(), error: Option[String] = None)

/** Context keeps state of interpretation
  * @param ids - mapping identifiers to its values
  */
case class Context(ids: Map[String, ExprValue] = Map())

class Interpreter {
  def run(prog: Program): Result = {
    // Iterates over program statements one-by-one and execute them
    val (_, result) = prog.stmts.foldLeft((Context(), Result())) { case (acc, stmt) =>
      val (ctx, res) = acc
      stmt match {
        // Print a string like : print "Hello, World!"
        case Print(str) => (ctx, res.copy(output = res.output :+ str))
        // Evaluate the expression, convert result to a string and print it
        case Out(expr) =>
          val exprResult = Expr.calc(expr, ctx)
          exprResult match {
            case ExprResult(Some(value), _) => (ctx, res.copy(output = res.output :+ value.toString))
            case ExprResult(None, error) => (ctx, res.copy(error = error))
          }
        // Define new variable (override old one), get its value eagerly
        // and keep it in the context
        case VarDef(id, expr) =>
          val exprResult = Expr.calc(expr, ctx)
          exprResult match {
            case ExprResult(Some(value), _) => (ctx.copy(ids = ctx.ids.updated(id, value)), res)
            case ExprResult(None, error) => (ctx, res.copy(error = error))
          }
      }
    }

    result
  }
}
