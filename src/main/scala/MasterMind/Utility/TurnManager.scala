package MasterMind.Utility

import akka.actor.typed.ActorRef

import scala.util.Random


class TurnManager(var players: Seq[ActorRef[Msg]],var index:Int = 0) {
  def nextPlayer: ActorRef[Msg] = index match {
    case players.size-1 =>
      nextTurnOrder()
      players(index)
    case _ => players(index)
    }

  private def nextTurnOrder() : Unit = {
    index = 0
    players = Random.shuffle(players)
  }
  def playerTurnEnd():Unit = index+=1
}
object TurnManager{
  def apply(players: Seq[ActorRef[Msg]], index: Int = 0): TurnManager = new TurnManager(players, index)
}
