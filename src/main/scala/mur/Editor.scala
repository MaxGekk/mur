package mur

import java.awt.event.{ActionEvent, ActionListener}

import scala.swing._
import scala.swing.event._

object Editor extends SimpleSwingApplication {
  val text = new TextArea(Main.text)
  val output = new TextArea("No errors\n")
  val interpreter = new Interpreter()
  def top = new MainFrame{
    title = "MuR Editor"
    preferredSize = new Dimension(500, 500)
    contents = new BoxPanel(Orientation.Vertical) {
      contents += new ScrollPane(text)
      contents += new ScrollPane(output)
    }
    listenTo(text.keys)

    val timerListener = new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent) = {
        Parsers.parse(text.text) match {
          case Left(err) => output.append("Parsing error: " + err + "\n")
          case Right(prog) =>
            val result = interpreter.run(prog)
            result match {
              case Result(out, None) => output.append(out.mkString + "\n")
              case Result(_, Some(err)) => output.append("Error: " + err + "\n")
            }
        }
      }
    }
    val timer = new javax.swing.Timer(1000, timerListener)
    timer.setRepeats(false)
    reactions += {
      case _: KeyTyped => timer.restart()
    }
  }
}
