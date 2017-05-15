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
      val outstr = try {
        Parsers.parse(text) match {
          case Left(err) => "Parsing error: " + err
          case Right(prog) =>
            val result = interpreter.run(prog)
            result match {
              case Result(out, None) => out.mkString
              case Result(_, Some(err)) => "Error: " + err
            }
        }
      } catch {
        case e: Throwable => "Exception:" + e.toString
      }
      counter += 1
      Swing.onEDT{ Editor.output.append(s"[$counter] $outstr\n") }
  }
}

object Editor extends SimpleSwingApplication {
  val input = new TextArea(Main.text)
  val output = new TextArea("[0] No errors\n")
  val system = ActorSystem("mur-editor")
  val WORKERS_AMOUNT = 2
  val workers = system.actorOf(
    props = SmallestMailboxPool(WORKERS_AMOUNT).
      withSupervisorStrategy(OneForOneStrategy(-1, Duration.Inf) { case _ => Restart }).
      withResizer(DefaultResizer(lowerBound = WORKERS_AMOUNT, upperBound = 2 * WORKERS_AMOUNT)).
      props(Props(classOf[Worker])),
    name = "worker"
  )

  def top = new MainFrame{
    title = "MuR Editor"
    preferredSize = new Dimension(500, 500)
    contents = new BoxPanel(Orientation.Vertical) {
      contents += new ScrollPane(input)
      contents += new ScrollPane(output)
    }
    listenTo(input.keys)

    val timerListener = new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent) = {
        workers ! NewInput(input.text)
      }
    }
    val timer = new javax.swing.Timer(1000, timerListener)
    timer.setRepeats(false)
    reactions += {
      case _: KeyTyped => timer.restart()
    }
  }
}
