package MasterMind.Controller

import MasterMind.Utility._
import akka.actor.typed.ActorRef
import scala.util.Random

case class GameController(private var codeLength: Int, private var sendGuessToOthers: Boolean, private var players: Map[ActorRef[Msg], Option[Code]]) {
  var playersTurn: Int = -1

  def getCodeLength: Int = codeLength
  def getSendGuessToOthers: Boolean = sendGuessToOthers

  def setPlayerCode(player: ActorRef[Msg], code: Code): Boolean = ???

  def getCurrentPlayer: Int = playersTurn
  def getAllPlayers: List[ActorRef[Msg]] = players.keys.toList
  def getEnemies(player: ActorRef[Msg]): List[ActorRef[Msg]] = players.keys.toList.filter(_ != player)

  def goNext(): Boolean = {
    if(playersTurn == -1 || playersTurn == players.size - 1) {
      playersTurn = 0
      players = Random.shuffle(players)
      true
    }
    else {
      playersTurn = playersTurn + 1
      false
    }
  }

  def guess(player: ActorRef[Msg], guess: Code) = ???
  def tryToWin(player: ActorRef[Msg], guess: Map[ActorRef[Msg], Code]) = ???
}

object GameController {
  def apply(codeLength: Int, sendGuessToOthers: Boolean, players: List[ActorRef[Msg]]): GameController = new GameController(codeLength, sendGuessToOthers, players.map(_ -> Option.empty).toMap)
}