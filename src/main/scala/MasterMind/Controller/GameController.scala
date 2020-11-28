package MasterMind.Controller

import akka.actor.{AbstractActor, ActorRef}

class GameController extends AbstractActor {

  /*Reference to the referee*/
  def referee : ActorRef = ???

  override def createReceive(): AbstractActor.Receive = ???

  /*Used for instance the game */
  def notInGame(): Receive = ???

  /*Used to stop the game if necessary*/
  def inGame(): Receive = ???

  /*Used for log the game's chat*/
  def logChat(msg: Object): Unit = msg.isInstanceOf match {
    case "GameStartedMsg" => //
    case "PlayerTurnMsg" => //
    case "GuessMsg" => //
    case "VictoryMsg" => //
    case "VictoryConfirm" => //
    case "GameStop" => //
  }

  /*Used for method notInGame when we need to instance the game*/
  def onCreateGame: Unit = ???

  /*Used when the game need to be stopped from the method inGame*/
  def onStopGame: Unit = ???
}

object GameControllerObj extends GameController {
  def apply(): GameController = new GameController()
}
