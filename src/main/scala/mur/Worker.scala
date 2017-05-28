package mur

import java.util.concurrent.Executors

import akka.actor.{Actor, OneForOneStrategy, Props}
import akka.actor.SupervisorStrategy.Restart
import akka.routing.{DefaultResizer, SmallestMailboxPool}
import mur.Editor.config

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.swing.Swing
import scala.util.parsing.input.Position

// A message with new entered text
case class NewInput(text: String)

class Worker extends Actor {
  var counter = 0L

  def receive = {
    case NewInput(text) =>
      val (outstr, error, end) = try {
        implicit val ec = Worker.threadPool
        val future = Future {
          Parsers.parse(text) match {
            case Left(err) => ("Parsing error " + err, Some(err), None)
            case Right(prog) =>
              val result = Interpreter.run(prog)
              result match {
                case Right(out) => (out.mkString, None, None)
                case Left(err) => ("Error " + err, Some(err), err.end(prog))
              }
          }
        }
        Await.result(future, Duration.Inf)
      } catch {
        case e: Throwable => ("Exception: " + e.toString, None, None)
      }
      counter += 1
      Worker.output(counter, outstr, error, end)
  }
}

object Worker {
  def settings = {
    val WORKERS_AMOUNT = config.getInt("akka.workers-amount")

    SmallestMailboxPool(WORKERS_AMOUNT).
      withSupervisorStrategy(OneForOneStrategy(-1, Duration.Inf) { case _ => Restart }).
      withResizer(DefaultResizer(
        lowerBound = WORKERS_AMOUNT,
        upperBound = WORKERS_AMOUNT
      )).
      props(Props(classOf[Worker]))
  }

  def output(counter: Long, outstr: String, error: Option[mur.Error], end: Option[Position]): Unit = {
    Swing.onEDT {
      Editor.output.peer.setText(s"[$counter] $outstr\n")

      val highlighter = Editor.input.peer.getHighlighter()
      highlighter.removeAllHighlights()
      error foreach { e =>
        import javax.swing.text.DefaultHighlighter
        import java.awt.Color
        def offset(pos: Position): Int = {
          Editor.input.peer.getLineStartOffset(pos.line - 1) + pos.column - 1
        }
        val painter = new DefaultHighlighter.DefaultHighlightPainter(Color.ORANGE)
        highlighter.addHighlight(
          offset(e.pos),
          end map (offset(_)) getOrElse Editor.input.peer.getLineEndOffset(e.pos.line - 1),
          painter
        )
      }
    }
  }
  val threadPool = {
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(
      config.getInt("map-reduce.thread-pool-size")
    ))
  }
}

