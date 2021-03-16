package MasterMind.View

import java.awt.Color

import MasterMind.Model.GameController
import MasterMind.Utility.{Code, GuessMsg, InitializeControllerMsg, Msg}
import akka.actor.typed.ActorSystem
import javax.swing.border.LineBorder

import scala.collection.mutable.ListBuffer
import scala.util.control.Exception.allCatch
import scala.swing.event.ButtonClicked
import scala.swing.{BoxPanel, Button, CheckBox, Dialog, Dimension, FlowPanel, Font, Label, MainFrame, Orientation, RadioButton, ScrollPane, TextArea, TextField}

object GUI extends MainFrame {

  val gameSystem: ActorSystem[Msg] = ActorSystem(GameController(), "GameSystem")
  var gameBoard: GameBoard = _
  var humanPanel: HumanPanel = _
  var codeLength: Int = _

  def top: MainFrame = new MainFrame() {
    startGame().open()
  }

  override def closeOperation(): Unit = {
    super.closeOperation()
  }

  def logChat(msg: String) : Unit = {
    gameBoard.logChat.text += "\n> " + msg
  }

  def isNumber(s: String): Boolean = (allCatch opt s.toDouble).isDefined
}

/**
 * Representing the initial dialog before starting the game
 */
class startingGameDialog extends Dialog {
  val numPlayers: TextField = new TextField(10)
  val textCodeLength: TextField = new TextField(10)
  val humanCheck: CheckBox = new CheckBox("Do you want a human player? ") /*{
    reactions += {
      case ButtonClicked(_) => if(humanCheck.selected) humanSelectNumberPanel.visible = true else humanSelectNumberPanel.visible = false
    }
  }*/
  val startButton: Button = new Button("START") {
    reactions += {
      case ButtonClicked(_) =>
      if (GUI.isNumber(numPlayers.text)) {
        if (GUI.isNumber(textCodeLength.text)) {
          GUI.codeLength = textCodeLength.text.toInt
          close()
          var isPresent: Boolean = false
          if (humanCheck.selected) {
            isPresent = true
          }
          GUI.gameBoard = Game(isPresent, numPlayers.text.toInt)
          GUI.gameSystem ! InitializeControllerMsg(numPlayers.text.toInt, GUI.codeLength, isPresent, sharedResponses = false)
          GUI.gameBoard.open()
        } else {
          Dialog.showMessage(contents.head, "Select a valid code's length", "ERROR!", Dialog.Message.Info, null)
        }
      } else {
        Dialog.showMessage(contents.head, "Select a valid number of player", "ERROR!", Dialog.Message.Info, null)
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
    contents ++= Seq(humanCheck)
  }

  /*val humanSelectNumberPanel: BoxPanel = new BoxPanel(Orientation.Horizontal) {
    contents += new Label("Select your code: ")
    contents += new TextField(10)
    visible = false
  }*/

  contents = new BoxPanel(Orientation.Vertical) {
    contents += new FlowPanel() {
      contents ++= Seq(playerPanel, numberPanel, humanCheck, startButton) //humanSelectNumberPanel, startButton)
    }
  }

  override def closeOperation(): Unit = {
    super.closeOperation()
    System.exit(0)
  }

  title = "Selecting parameters"
  size = new Dimension(450, 150)
  this.peer.setLocationRelativeTo(null)
}

object startGame {
  def apply(): startingGameDialog = new startingGameDialog()
}


/**
 * Representing the Mastermind's board
 */
class GameBoard(withHuman: Boolean, nPlayers: Int) extends Dialog {
  val logChat: TextArea = new TextArea("GAME IS STARTED!")
  logChat.editable = false
  logChat.border = new LineBorder(Color.BLACK, 2)
  logChat.font = Font("monospaced", Font.Italic, 15)

  var startStop: Button = new Button("Stop") {
    reactions += {
      case ButtonClicked(_) => startStop.text match {
        case "Stop" => startStop.text ="Start"; println("Stop the game")
        case _ => startStop.text ="Stop"; println("Resume the game")
      }
    }
  }
  startStop.preferredSize = new Dimension(250,40)
  startStop.xLayoutAlignment = 0.5

  contents = new BoxPanel(Orientation.Vertical) {
    if(withHuman) GUI.humanPanel = HumanPanelObj(nPlayers); contents += GUI.humanPanel
    contents += new ScrollPane(logChat)
    contents += startStop
  }

  title = "Mastermind's GameBoard"
  size = new Dimension(700, 500)
  this.peer.setLocationRelativeTo(null)

  override def closeOperation(): Unit = {
    super.closeOperation()
    System.exit(0)
  }
}

object Game {
  def apply(withHuman: Boolean, nPlayers: Int): GameBoard = new GameBoard(withHuman, nPlayers)
}

class HumanPanel(nPlayers: Int) extends BoxPanel(Orientation.Vertical) {
  var radioPlayer = new ListBuffer[RadioButton]()
  val selectNumber: TextField = new TextField() {
    maximumSize = new Dimension(150,40)
  }
  val sendGuess: Button = new Button("Send")

  contents += new BoxPanel(Orientation.Horizontal) {
    contents += new Label("Select a player: ")
    println(nPlayers)
    for (i <- 0 until nPlayers) {
      radioPlayer += new RadioButton(" Player" + i) {
        reactions += {
          case ButtonClicked(_) => for(a <- radioPlayer) a.selected = false; selected = true
        }
      }
    }
    contents ++= radioPlayer
  }
  contents += new BoxPanel(Orientation.Horizontal) {
    contents += new Label("Select a number:  ")
    contents += selectNumber
    contents += sendGuess
  }

  def getGuess: Map[String, Code] = {
    var guess: Map[String, Code] = Map.empty
    val player: RadioButton = isPlayerSelected
    if(player != null) {
      if(isGuessValid(selectNumber.text)) {
        guess += (player.text -> Code(selectNumber.text))
        guess
      } else {
        Dialog.showMessage(contents.head, "Select a valid number with specified length", "ERROR!", Dialog.Message.Info, null)
        Map.empty
      }
    } else {
      Dialog.showMessage(contents.head, "Select a player", "ERROR!", Dialog.Message.Info, null)
      Map.empty
    }
  }

  def isPlayerSelected: RadioButton = {
    for(i <- radioPlayer) {
      if(i.selected) return i
    }
    null
  }

  def isGuessValid(code: String) : Boolean = GUI.isNumber(code) && code.length == GUI.codeLength
}

object HumanPanelObj {
  def apply(nPlayers: Int): HumanPanel = new HumanPanel(nPlayers)
}
