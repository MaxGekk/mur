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
        val parsed = Parsers.parse(text.text)
        if (parsed.successful) {
          val result = interpreter.run(parsed.get)
          if (!result.error.isEmpty) {
            output.append("Error: " + result.error.get)
          } else if (!result.output.isEmpty) {
            output.append(result.output.mkString)
          }
          output.append("\n")
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
