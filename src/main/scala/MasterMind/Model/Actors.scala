package MasterMind.Model

import akka.actor.{Actor, ActorRef}
import scala.util.Random
import MasterMind.Utility.{AllGuessesMsg, Code, GuessResponseMsg, Response, StartGameMsg, YourTurnMsg}

/**
 * Two type of players involved : AI players & human player
 */
sealed trait Player extends Actor {

  def opponents: List[ActorRef]
  def currentOpponentId: Int
  def opponentsCodes: Map[ActorRef, Int]
  def guessAllCodes: Boolean

  def getOpponent(): ActorRef
  /**
   * Player receive only 3 types of msgs:
   * - someone tried to guess its number
   * - it's his turn
   * - game's starting
   */
   def receive: Receive = {
     case msg : GuessResponseMsg => playerGuess(msg.getGuess,msg.getResponse)
     case _ : YourTurnMsg => playTurn()
     case _ : StartGameMsg => secretNumber
   }

  /**
   * Respond to another player's guess
   * @param guess guessed value
   * @param response response
   */
  def playerGuess(guess:Code,response:Response)

  /**
   * try to win boy
   */
  def playTurn()
  /**
   * Player's secret number, evaluated once on first invocation
   */
  lazy val secretNumber : Int => Int = (digits:Int ) =>Random.nextInt(math.pow(10,digits).toInt-1)
}

class UserPlayer extends Player {

  override def opponents: List[ActorRef] = ???
  override def currentOpponentId: Int = ???
  override def opponentsCodes: Map[ActorRef, Int] = ???
  override def guessAllCodes: Boolean = ???

  override  def playerGuess(guess:Code,response:Response): Unit = {
    //TODO
  }
  override def playTurn(): Unit = {
    //TODO
  }
  override def getOpponent(): ActorRef = ???
}

class AIPlayer extends Player {
  override def opponents: List[ActorRef] = ???
  override def currentOpponentId: Int = ???
  override def opponentsCodes: Map[ActorRef, Int] = ???
  override def guessAllCodes: Boolean = ???

  override def getOpponent(): ActorRef = ???

  override def playerGuess(guess:Code,response:Response): Unit = {
    //TODO
  }

  override def playTurn(): Unit = {
    //TODO
  }
}

/**
 * Game's corrupted referee
 */
trait Referee extends Actor {
  /**
   * Receives two messages:
   * - someone tries to win
   * - game is starting
   * @return
   */
   def receive: Receive = {
     case msg : AllGuessesMsg => checkWin(msg.getGuesses)
     case _ : StartGameMsg => generateTurns()
  }

  /**
   * Check if a player actually won
   * @param value values to check
   */
  def checkWin(value: Map[ActorRef,Code])

  /**
   * Create turns
   */
  def generateTurns()
}
