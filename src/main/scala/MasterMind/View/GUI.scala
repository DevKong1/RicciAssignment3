package MasterMind.View

import java.awt.Color

import MasterMind.Model.GameController
import MasterMind.Utility.InitializeControllerMsg
import akka.actor.typed.ActorSystem
import javax.swing.border.LineBorder

import scala.util.control.Exception.allCatch
import scala.swing.event.ButtonClicked
import scala.swing.{BoxPanel, Button, CheckBox, Dialog, Dimension, FlowPanel, GridPanel, Label, MainFrame, Orientation, TextArea, TextField}

object GUI extends MainFrame {

  val gameSystem = ActorSystem(GameController(), "GameSystem")

  def top: MainFrame = new MainFrame() {
    startGame().open()
  }

  override def closeOperation(): Unit = {
    super.closeOperation()
  }

  def logChat(msg: String) : Unit = {}

}

/**
 * Representing the initial dialog before starting the game
 */
class startingGameDialog extends Dialog {
  val numPlayers: TextField = new TextField(10)
  val textCodeLength: TextField = new TextField(10)
  val humanCheck: CheckBox = new CheckBox("Do you want a human player? ")
  val startButton: Button = new Button("START") {
    reactions += {
      case ButtonClicked(_) =>
      isNumber(numPlayers.text) match {
        case true => isNumber(textCodeLength.text) match {
          case true => close()
            GUI.gameSystem ! InitializeControllerMsg(numPlayers.text.toInt,textCodeLength.text.toInt,withHuman = false,sharedResponses = false)
            Game(numPlayers.text.toInt).open()
          case _ => Dialog.showMessage(contents.head, "Select a valid code's length", "ERROR!", Dialog.Message.Info, null)
        }
        case _ => Dialog.showMessage(contents.head, "Select a valid number of player", "ERROR!", Dialog.Message.Info, null)
      }
    }
  }

  val playerPanel: BoxPanel = new BoxPanel(Orientation.Horizontal) {
    contents ++= Seq(new Label("Select number of players: "), numPlayers)
  }

  val numberPanel: BoxPanel = new BoxPanel(Orientation.Horizontal) {
    contents ++= Seq(new Label("Select content's length: "), textCodeLength)
  }

  val humanPanel: BoxPanel = new BoxPanel(Orientation.Horizontal) {
    contents += humanCheck
  }

  contents = new BoxPanel(Orientation.Vertical) {
    contents += new FlowPanel() {
      contents ++= Seq(playerPanel, numberPanel, humanCheck, startButton)
    }
  }

  override def closeOperation(): Unit = {
    super.closeOperation()
  }

  def isNumber(s: String): Boolean = (allCatch opt s.toDouble).isDefined

  title = "Starting Game!"
  size = new Dimension(450, 150)
  this.peer.setLocationRelativeTo(null)
}

object startGame {
  def apply(): startingGameDialog = new startingGameDialog()
}


/**
 * Representing the Mastermind's board
 */
class GameBoard(nPlayers: Int) extends Dialog {
  val logChat: TextArea = new TextArea("GAME IS STARTED!")
  logChat.editable = false
  logChat.border = new LineBorder(Color.BLACK, 2)

  /*
  val playerPanel = new BoxPanel(Orientation.Vertical)

  for( i <- 0 until nPlayers) {
    playerPanel.contents += new GridPanel(1,2) {
      contents += new Label("Player " + i)
      i match {
          case 0 => contents += new Label("My turn")
          case _ => contents += new Label("My turn") {
            visible = false
          }
        }
      vGap = 10
    }
    //println("Hello Player " + i)
  }
  */

  contents = new BoxPanel(Orientation.Vertical) {
    contents ++= Seq(logChat)
  }

  size = new Dimension(800, 400)
  this.peer.setLocationRelativeTo(null)

  override def closeOperation(): Unit = {
    super.closeOperation()
  }
}

object Game extends {
  def apply(nPlayers: Int): GameBoard = new GameBoard(nPlayers)
}


