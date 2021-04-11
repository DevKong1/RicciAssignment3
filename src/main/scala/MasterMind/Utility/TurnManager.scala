package MasterMind.Utility

import akka.actor.typed.ActorRef

import scala.util.Random


class TurnManager(var players: Seq[ActorRef[Msg]],var index:Int) {


  def nextPlayer: ActorRef[Msg] = if (index == players.size-1) {
    println("Completed a whole turn!")
    nextTurnOrder()
    players(index)
  } else {
    index += 1;
    players(index)
  }

  private def nextTurnOrder() : Unit = {
    index = 0
    players = Random.shuffle(players)
  }
  def playerTurnEnd():Unit = index+=1
  def setPlayers(newPlayers:Seq[ActorRef[Msg]]): Unit = players = newPlayers
  def removePlayer(player:ActorRef[Msg]) : Unit = players filterNot Seq(player).contains
}
object TurnManager{
  def apply(): TurnManager = new TurnManager(Seq.empty,-1)
}
