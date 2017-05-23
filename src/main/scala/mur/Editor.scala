package mur

import java.awt.event.{ActionEvent, ActionListener}

import akka.actor.{Actor, ActorSystem, OneForOneStrategy, Props}
import akka.actor.SupervisorStrategy.Restart
import akka.routing.{DefaultResizer, SmallestMailboxPool}

import scala.concurrent.duration.Duration
import scala.swing._
import scala.swing.event._

case class NewInput(text: String)

class Worker extends Actor {
  val interpreter = new Interpreter()
  var counter = 0L

  def receive = {
    case NewInput(text) =>
      val (outstr, error) = try {
        Parsers.parse(text) match {
          case Left(err) => ("Parsing error: " + err, Some(err))
          case Right(prog) =>
            val result = interpreter.run(prog)
            result match {
              case Result(out, None) => (out.mkString, None)
              case Result(_, Some(err)) => ("Error: " + err, Some(err))
            }
        }
      } catch {
        case e: Throwable => ("Exception:" + e.toString, None)
      }
      counter += 1
      output(outstr, error)
  }

  def output(outstr: String, error: Option[mur.Error]): Unit = {
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

object Editor extends SimpleSwingApplication {
  val input = new TextArea(Main.text)
  val output = new TextArea("[0] No errors\n")
  val actorSystem = ActorSystem("mur-editor")
  val WORKERS_AMOUNT = 1
  val workers = actorSystem.actorOf(
    props = SmallestMailboxPool(WORKERS_AMOUNT).
      withSupervisorStrategy(OneForOneStrategy(-1, Duration.Inf) { case _ => Restart }).
      withResizer(DefaultResizer(lowerBound = WORKERS_AMOUNT, upperBound = 2 * WORKERS_AMOUNT)).
      props(Props(classOf[Worker])),
    name = "worker"
  )

  def top = new MainFrame {
    title = "MuR Editor"
    preferredSize = new Dimension(500, 500)
    contents = new BoxPanel(Orientation.Vertical) {
      contents += new ScrollPane(input)
      contents += new ScrollPane(output)
    }
    listenTo(input.keys)

    val timerListener = new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent) = {
        workers ! NewInput(input.peer.getText)
      }
    }
    val timer = new javax.swing.Timer(1000, timerListener)
    timer.setRepeats(false)
    reactions += {
      case _: KeyTyped => timer.restart()
    }
  }

  override def shutdown(): Unit = {
    actorSystem.stop(workers)
    actorSystem.terminate()
  }
}

