package MasterMind.Controller

import akka.actor.{AbstractActor, Actor, ActorRef, Props}
import MasterMind.Utility._
import MasterMind.Model._

//TODO: UPDATE WITH INTERFACE ACTOR AND NOT WITH ABSTRACT ACTOR
class GameController extends Actor {

  /*Reference to the referee*/
  private var referee: Option[ActorRef] = Option.empty
  private var players: Option[List[ActorRef]] = Option.empty

  private var codeLength: Option[Int] = Option.empty
  private var sharedResponses: Option[Boolean] = Option.empty

  override def receive: Receive = ???

  // Behavior when the game is not playing
  def notInGame(): Receive = ???

  // Behavior when the game is playing
  def inGame(): Receive = ???

  // Used to log the game's chat into the GUI
  def logChat(msg: Msg): Unit = msg match {
    // NO StartGame or StopGame Msg since it's the controller which executes those commands
    // Each of this will do an ex. GUI.logChat("Player " + GuessMsg.Player.Name + " tried to guess ....
    case Msg : TurnOrderMsg => ???
    case Msg : YourTurnMsg => ???
    case Msg : GuessMsg => ???
    case Msg : AllGuessesMsg => ???
    case Msg : GuessResponseMsg => ???
    case Msg : VictoryConfirmMsg => ???
    case Msg : VictoryDenyMsg => ???
    case _ => ???
  }

  // Log in chat commands executed by the controller
  def logChat(msg: String): Unit = ???

  // Create a new game
  def onCreateGame(msg: InitializeControllerMsg): Unit = {
    codeLength = Option(msg.getLength)
    sharedResponses = Option(msg.getResponses)

    referee = Option(context.actorOf(Props[Referee]))
    var playersList: List[ActorRef] = List.tabulate(if(msg.getHuman) msg.getPlayers - 1 else msg.getPlayers)(n => context.actorOf(Props[AIPlayer], "Player" + n))
    if (msg.getHuman) playersList = context.actorOf(Props[AIPlayer], "HumanPlayer") :: playersList
    players = Option(playersList)
    referee.get.tell(new StartGameMsg(), self)
    logChat("The game has started")
    context.become(inGame())
  }

  // Terminate the current game
  def onStopGame(): Unit = {
    if(referee.isDefined) {
      referee.get.tell(new StopGameMsg(), self)
      logChat("The game has been stopped")
      context.become(notInGame())
    }
  }

}

object GameControllerObj extends GameController {
  def apply(): GameController = new GameController()
}
