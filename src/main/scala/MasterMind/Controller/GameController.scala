package MasterMind.Controller

import akka.actor.{Actor, ActorRef, Props}
import MasterMind.Utility._
import MasterMind.Model._
import MasterMind.Model.Referee
import MasterMind.View.GUI

//TODO: UPDATE WITH INTERFACE ACTOR AND NOT WITH ABSTRACT ACTOR
class GameController(GUI: GUI) extends Actor {
  
  /*Reference to the referee*/
  private var referee: Option[ActorRef] = Option.empty
  private var players: Option[List[ActorRef]] = Option.empty

  private var codeLength: Option[Int] = Option.empty
  private var sharedResponses: Option[Boolean] = Option.empty

  override def receive: Receive = noGame()

  // Behavior when the game is not playing
  def noGame(): Receive = {
    case msg: InitializeControllerMsg => onCreateGame(msg)
    case _ => GUI.logChat("Controller received an unexpected Msg")
  }

  // Behavior when the game is playing
  def inGame(): Receive = {
    case msg: StopGameMsg => onStopGame(msg)
    case msg: Msg => logChat(msg)
    case _ => GUI.logChat("Controller received an unexpected type of msg")
  }

  // Used to log the game's chat into the GUI
  def logChat(msg: Msg): Unit = msg match {
    // NO StartGame or StopGame Msg since it's the controller which executes those commands
    // Each of this will do an ex. GUI.logChat("Player " + GuessMsg.Player.Name + " tried to guess ....
    case msg : TurnOrderMsg =>  GUI.logChat("The order for this round is " + msg.getTurns.mkString(" -> "))
    case msg : YourTurnMsg => GUI.logChat(msg.getPlayer + " it's your turn!")
    case msg : GuessMsg => GUI.logChat("Trying to guess " + msg.getPlayer + " code -> " + msg.getGuess)
    case msg : AllGuessesMsg => GUI.logChat("Trying to guess all codes:\n" + msg.getGuesses.map(x => x._1 + " -> " + x._2).mkString(",\n"))
    case msg : GuessResponseMsg => GUI.logChat("Response from " + msg.getPlayer + " -> " + msg.getResponse)
    case msg : VictoryConfirmMsg => GUI.logChat(msg.getPlayer + " just won the game! YAY!")
    case msg : VictoryDenyMsg => GUI.logChat(msg.getPlayer + " failed miserably his attempt at winning the game.")
    case _ => GUI.logChat("Controller received an unexpected Msg")
  }

  // Create a new game
  def onCreateGame(msg: InitializeControllerMsg): Unit = {
    codeLength = Option(msg.getLength)
    sharedResponses = Option(msg.getResponses)

    referee = Option(context.actorOf(Props[Referee]))
    var playersList: List[ActorRef] = List.tabulate(if(msg.getHuman) msg.getPlayers - 1 else msg.getPlayers)(n => context.actorOf(Props[AIPlayer], "Player" + n))
    if (msg.getHuman) playersList = context.actorOf(Props[AIPlayer], "HumanPlayer") :: playersList
    players = Option(playersList)
    referee.get.tell(new StartGameMsg(), self)
    GUI.logChat("The game has started")
    context.become(inGame())
  }

  // Terminate the current game
  def onStopGame(msg: StopGameMsg): Unit = {
    if(referee.isDefined) {
      referee.get.tell(msg, self)
      GUI.logChat("The game has been stopped")
      context.become(noGame())

      referee = Option.empty
      players = Option.empty
      codeLength = Option.empty
      sharedResponses = Option.empty
    }
  }

}

object GameController {
  def apply(GUI: GUI): GameController = new GameController(GUI)
}