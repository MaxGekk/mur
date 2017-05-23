package mur

import java.awt.event.{ActionEvent, ActionListener}
import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import scala.swing._
import scala.swing.event._

object Editor extends SimpleSwingApplication {
  val config = ConfigFactory.load
  val input = new TextArea(Main.text)
  val output = new TextArea("[0] No errors\n")
  val actorSystem = ActorSystem("mur-editor")
  val workers = actorSystem.actorOf(props = Worker.settings, name = "worker")

  def top = new MainFrame {
    title = "MuR Editor"
    preferredSize = new Dimension(
      config.getInt("gui.window-height"),
      config.getInt("gui.window-width")
    )
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
    val timer = new javax.swing.Timer(config.getInt("gui.processing-delay"), timerListener)
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

