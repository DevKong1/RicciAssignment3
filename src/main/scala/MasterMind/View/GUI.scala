package MasterMind.View

import scala.swing.{BoxPanel, Button, Dialog, Dimension, FlowPanel, Label, MainFrame, Orientation, TextField}

object GUI extends MainFrame {

  def top: MainFrame = new MainFrame() {
    startGame().open()
  }

  override def closeOperation(): Unit = {
    super.closeOperation()
  }

  def logChat(msg: String) : Unit = {}

}

class startingGameDialog extends Dialog {
  title = "Starting Game!"

  val numPlayers: TextField = new TextField(10)
  val numNumbers: TextField = new TextField(10)
  val startButton: Button = new Button("START")

  val playerPanel: BoxPanel = new BoxPanel(Orientation.Horizontal) {
    contents ++= Seq(new Label("Select number of players: "), numPlayers)
  }

  val numberPanel: BoxPanel = new BoxPanel(Orientation.Horizontal) {
    contents ++= Seq(new Label("Select how many numbers you have to guess: "), numNumbers)
  }

  contents = new BoxPanel(Orientation.Vertical) {
    contents += new FlowPanel() {
      contents ++= Seq(playerPanel, numberPanel, startButton)
    }
  }

  size = new Dimension(450, 150)
  this.peer.setLocationRelativeTo(null)
}

object startGame extends startingGameDialog {
  def apply(): startingGameDialog = new startingGameDialog()
}
