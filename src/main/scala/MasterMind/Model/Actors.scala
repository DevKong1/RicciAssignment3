package MasterMind.Model

import akka.actor.ActorRef
import MasterMind.Utility.{AllGuessesMsg, Code, CodeBreakerImpl, GuessMsg, GuessResponseMsg, Msg, StartGameMsg, StopGameMsg, VictoryConfirmMsg, YourTurnMsg}
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

import scala.concurrent.Future

/**
 * Two type of players involved : AI players & human player, they're represented as FSM
 */
sealed trait Player {

  def myCode: Code
  def codeBreaker : CodeBreakerImpl
  def respond(player:ActorRef,guess:Code): Unit
  def guess(player:ActorRef,guess:Code): Unit

  /**
   * Player's secret number, evaluated once on first invocation
   *
  lazy val secretNumber : Int => Int = (digits:Int ) => Random.nextInt(math.pow(10,digits).toInt-1)*/
    /**
     * First state: idle
     */
  def idle(data:Code) : Behavior[Msg] = {
    Behaviors.receive{
      case (_,StartGameMsg()) =>  /*generate random number*/ waitTurn(data)
    }
  }
  def idle() : Behavior[Msg] = {
    Behaviors.receive{
      case (_,StartGameMsg()) =>  /*generate random number*/ waitTurn(null/*NEED TO declare new code*/)
    }
  }

  /**
   * State in which player's are waiting for their turn
   * @param mySecretNumber my secret number
   * @return //
   */
  def waitTurn(mySecretNumber:Code): Behavior[Msg] = Behaviors.receive{
      case (_,_ : YourTurnMsg) =>  myTurn(mySecretNumber)
      case (_, GuessMsg(actor,code)) => respond(actor,code); Behaviors.same
      case (_, GuessResponseMsg(player, guess, response)) => /*should update list of other guesses*/ Behaviors.same
  }

  /**
   * State representing player's turn and actions
   * @param mySecretNumber my secret number
   * @return
   */
  def myTurn(mySecretNumber:Code): Behavior[Msg] = {
    Behaviors.receive{
      case (_, GuessResponseMsg(player, guess, response)) =>/*should update list of my guesses*/ /*send turn end*/Behaviors.same
      case (_,_:VictoryConfirmMsg) => /*I WON*/ idle(mySecretNumber)
    }
  }
}

class UserPlayer extends Player {
  override def myCode: Code = ???
  override def codeBreaker: CodeBreakerImpl = ???
  override def respond(player: ActorRef, guess: Code): Unit = ???
  override def guess(player: ActorRef, guess: Code): Unit = ???
}
object UserPlayer {
  def apply(): Behavior[Msg] = new UserPlayer().idle(null)
}
class AIPlayer extends Player {
  override def myCode: Code = ???
  override def codeBreaker: CodeBreakerImpl = ???
  override def respond(player: ActorRef, guess: Code): Unit = ???
  override def guess(player: ActorRef, guess: Code): Unit = ???
}
object AIPlayer {
  def apply(): Behavior[Msg] = new AIPlayer().idle(null)

}
/**
 * Game's corrupted referee
 */
sealed trait Referee {


  /**
   * Check if a player actually won
   * @param value values to check
   */
  def checkWin(value: Map[ActorRef,Code]): Unit
  /**Determines play order for a turn*/
  def generateTurns():Set[ActorRef]
  /**Allows a player to play his turn*/
  def playTurn(play: ActorRef): Future[ActorRef]
  /**
   *
   */
  def idle() : Behavior[Msg] = {
    Behaviors.receive{
      case (_,StartGameMsg()) => refereeTurn()
    }
  }
  def refereeTurn() : Behavior[Msg] = {
    /*play turns*/
    Behaviors.receive{
      case (_,StopGameMsg()) => idle()
      case (_,AllGuessesMsg(_)) => /*checkWin*/ Behaviors.same
    }
  }
}

class RefereeImpl extends Referee{
  /**
   * Check if a player actually won
   *
   * @param value values to check
   */
  override def checkWin(value: Map[ActorRef, Code]): Unit = ???
  /**Determines play order for a turn*/
  override def generateTurns(): Set[ActorRef] = ???
  /** Allows a player to play his turn */
  override def playTurn(play: ActorRef): Future[ActorRef] = ???
}
object Referee{
  def apply(): Behavior[Msg] = new RefereeImpl().idle()
}
