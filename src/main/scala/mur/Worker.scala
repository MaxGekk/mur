package mur

import akka.actor.{Actor, OneForOneStrategy, Props}
import akka.actor.SupervisorStrategy.Restart
import akka.routing.{DefaultResizer, SmallestMailboxPool}
import mur.Editor.config

import scala.concurrent.duration.Duration
import scala.swing.Swing

// A message with new entered text
case class NewInput(text: String)

class Worker extends Actor {
  var counter = 0L

  def receive = {
    case NewInput(text) =>
      val (outstr, error) = try {
        Parsers.parse(text) match {
          case Left(err) => ("Parsing error: " + err, Some(err))
          case Right(prog) =>
            val result = Interpreter.run(prog)
            result match {
              case Right(out) => (out.mkString, None)
              case Left(err) => ("Error: " + err, Some(err))
            }
        }
      } catch {
        case e: Throwable => ("Exception:" + e.toString, None)
      }
      counter += 1
      Worker.output(counter, outstr, error)
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

  def output(counter: Long, outstr: String, error: Option[mur.Error]): Unit = {
    Swing.onEDT {
      Editor.output.peer.setText(s"[$counter] $outstr\n")

      val highlighter = Editor.input.peer.getHighlighter()
      highlighter.removeAllHighlights()
      error foreach { e =>
        import javax.swing.text.DefaultHighlighter
        import java.awt.Color
        val painter = new DefaultHighlighter.DefaultHighlightPainter(Color.ORANGE)
        highlighter.addHighlight(
          Editor.input.peer.getLineStartOffset(e.line - 1) + e.column - 1,
          Editor.input.peer.getLineEndOffset(e.line - 1),
          painter
        )
      }
    }
  }
}

