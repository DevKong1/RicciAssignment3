package MasterMind.Controller

import MasterMind.Utility._
import akka.actor.typed.ActorRef

object GameController {
  private val codeLength = 0
  private val shareGuesses = false

  def getCodeLength: Int = codeLength
  def getShareGuesses: Boolean = shareGuesses

  def setPlayerCode(player: Nothing, code: Code): Boolean = ???

  def getCurrentPlayer: Nothing = ???
  def getAllPlayers: List[ActorRef[Msg]] = ???
  def getEnemies(player: Nothing): List[Nothing] = ???

  def goNext = ???

  def guess(player: Nothing, guess: Code) = ???
  def tryToWin(player: Nothing, guess: Map[ActorRef[Msg], Code]) = ???

}