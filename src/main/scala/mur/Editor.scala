package mur

import java.awt.event.{ActionEvent, ActionListener}

import scala.swing._
import scala.swing.event._

object Editor extends SimpleSwingApplication {
  val input = new TextArea(Main.text)
  val output = new TextArea("No errors\n")
  val interpreter = new Interpreter()
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
        val outstr = Parsers.parse(input.text) match {
          case Left(err) => "Parsing error: " + err
          case Right(prog) =>
            val result = interpreter.run(prog)
            result match {
              case Result(out, None) => out.mkString
              case Result(_, Some(err)) => "Error: " + err
            }
        }
        output.append(outstr + "\n")
      }
    }
    val timer = new javax.swing.Timer(1000, timerListener)
    timer.setRepeats(false)
    reactions += {
      case _: KeyTyped => timer.restart()
    }
  }
}
