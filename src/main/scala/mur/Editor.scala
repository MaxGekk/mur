package mur

import java.awt.event.{ActionEvent, ActionListener}

import scala.swing._
import scala.swing.event._

object Editor extends SimpleSwingApplication {
  val text = new TextArea(Main.text)
  val output = new TextArea("No errors")
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
        output.append("\n>>>\n")
        val parsed = Parsers.parse(text.text)
        if (parsed.successful) {
          val result = interpreter.run(parsed.get)
          if (!result.output.isEmpty) {
            output.append("Output:\n")
            output.append(result.output.mkString("\n"))
          }
          result.error.foreach("Error: " + output.append(_))
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
