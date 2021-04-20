package MasterMind.View

import java.awt.Color

import MasterMind.Model.GameController
import MasterMind.Utility.{Code, InitializeControllerMsg, Msg, StopGameMsg}
import akka.actor.typed.ActorSystem
import javax.swing.border.LineBorder

import scala.collection.mutable.ListBuffer
import scala.util.control.Exception.allCatch
import scala.swing.event.ButtonClicked
import scala.swing.{BoxPanel, Button, CheckBox, Dialog, Dimension, FlowPanel, Font, Label, MainFrame, Orientation, RadioButton, ScrollPane, TextArea, TextField}

/**
 * The GUI's MainFrame
 */
object GUI extends MainFrame {

  var gameSystem: ActorSystem[Msg] = _
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
  val humanCheck: CheckBox = new CheckBox("Do you want a human player? ")
  val shareGuessCheck: CheckBox = new CheckBox("Do you want to share all guesses?")

  val startButton: Button = new Button("START") {
    reactions += {
      case ButtonClicked(_) =>
        if (GUI.isNumber(numPlayers.text)) {
          if (GUI.isNumber(textCodeLength.text)) {
            GUI.codeLength = textCodeLength.text.toInt
            close()
            var isPresent: Boolean = false
            var sharedResponses: Boolean = false
            if (humanCheck.selected) isPresent = true
            if (shareGuessCheck.selected) sharedResponses = true
            GUI.gameSystem = ActorSystem(GameController(), "GameSystem")
            GUI.gameBoard = Game(isPresent, sharedResponses, numPlayers.text.toInt)
            GUI.gameSystem ! InitializeControllerMsg(numPlayers.text.toInt, GUI.codeLength, isPresent, sharedResponses)
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

  val checkPanel: BoxPanel = new BoxPanel(Orientation.Horizontal) {
    contents ++= Seq(humanCheck, shareGuessCheck)
  }

  contents = new BoxPanel(Orientation.Vertical) {
    contents += new FlowPanel() {
      contents ++= Seq(playerPanel, numberPanel, checkPanel, startButton)
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
 * The Mastermind's game board
 * @param withHuman
 * @param withSharing
 * @param nPlayers
 */
class GameBoard(withHuman: Boolean, withSharing: Boolean, nPlayers: Int) extends Dialog {
  val logChat: TextArea = new TextArea("GAME IS STARTED!")
  logChat.editable = false
  logChat.border = new LineBorder(Color.BLACK, 2)
  logChat.font = Font("monospaced", Font.Italic, 15)
  logChat.lineWrap = true

  var startStop: Button = new Button("Stop") {
    reactions += {
      case ButtonClicked(_) => startStop.text match {
        case "Stop" => startStop.text = "New Game"; GUI.gameSystem ! StopGameMsg()
        case _ => startStop.text = "Stop"; close(); GUI.top
      }
    }
  }
  startStop.preferredSize = new Dimension(250,40)
  startStop.xLayoutAlignment = 0.5

  contents = new BoxPanel(Orientation.Vertical) {
    if(withHuman) {GUI.humanPanel = HumanPanelObj(nPlayers); contents += GUI.humanPanel}
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
  def apply(withHuman: Boolean, withSharing: Boolean, nPlayers: Int): GameBoard = new GameBoard(withHuman, withSharing, nPlayers)
}

/**
 * Panel for human's interaction
 * @param nPlayers
 */
class HumanPanel(nPlayers: Int) extends BoxPanel(Orientation.Vertical) {
  var radioPlayer = new ListBuffer[RadioButton]()
  val selectNumber: TextField = new TextField() {
    maximumSize = new Dimension(150,40)
  }
  val sendGuess: Button = new Button("Send")

  contents += new BoxPanel(Orientation.Horizontal) {
    contents += new Label("Select a player: ")
    for (i <- 0 until nPlayers-1) {
      radioPlayer += new RadioButton(" Player" + i) {
        reactions += {
          case ButtonClicked(_) => for(a <- radioPlayer) a.selected = false; selected = true
        }
      }
    }
    contents ++= radioPlayer
  }

  contents += new BoxPanel(Orientation.Horizontal) {
    contents ++= Seq(new Label("Select a number:  "), selectNumber, sendGuess)
  }

  def getGuess: Map[String, Code] = {
    var guess: Map[String, Code] = Map.empty
    val player: RadioButton = isPlayerSelected
    if(player != null) {
      if(isGuessValid(selectNumber.text)) {
        guess += (player.text.replaceAll("\\s+", "") -> Code(selectNumber.text.length, selectNumber.text))
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

/**
 * Object of HumanPanel class
 */
object HumanPanelObj {
  def apply(nPlayers: Int): HumanPanel = new HumanPanel(nPlayers)
}
