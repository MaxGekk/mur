package mur

case class Result(output: Seq[String] = Seq(), error: Option[String] = None)
case class Context(ids: Map[String, ExprValue] = Map())

class Interpreter {
  def run(prog: Program): Result = {
    val (_, result) = prog.stmts.foldLeft((Context(), Result())) { case (acc, stmt) =>
      val (ctx, res) = acc
      stmt match {
        case Print(str) => (ctx, res.copy(output = res.output :+ str))
        case Out(expr) =>
          val exprResult = Expr.calc(expr, ctx)
          exprResult match {
            case ExprResult(Some(value), _) => (ctx, res.copy(output = res.output :+ value.toString))
            case ExprResult(None, error) => (ctx, res.copy(error = error))
          }
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
