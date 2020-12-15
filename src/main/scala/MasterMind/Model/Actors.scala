package MasterMind.Model

import MasterMind.Utility._
import MasterMind.View.GUI
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}

import scala.concurrent.Future
import scala.util.Random

/**
 * Two type of players involved : AI players & human player, they're represented as FSM
 */
sealed trait Player {

  def myCode: Code
  def codeBreaker : CodeBreakerImpl
  def respond(player:ActorRef[Msg] ,guess:Code): Unit
  def guess(player:ActorRef[Msg] ,guess:Code): Unit

  /**
   * Player's secret number, evaluated once on first invocation
   *
  lazy val secretNumber : Int => Int = (digits:Int ) => Random.nextInt(math.pow(10,digits).toInt-1)*/
    /**
     * First state: idle
     */
  def idle(data:Code) : Behavior[Msg] = {
    Behaviors.receive{
      case (_,StartGameMsg(_,_,_)) =>  /*generate random number*/ waitTurn(data)
    }
  }
  def idle() : Behavior[Msg] = {
    Behaviors.receive{
      case (_,StartGameMsg(_,_,_)) =>  /*generate random number*/ waitTurn(null/*NEED TO declare new code*/)
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
  override def respond(player: ActorRef[Msg], guess: Code): Unit = ???
  override def guess(player: ActorRef[Msg], guess: Code): Unit = ???
}
object UserPlayer {
  def apply(): Behavior[Msg] = new UserPlayer().idle(null)
}
class AIPlayer extends Player {
  override def myCode: Code = ???
  override def codeBreaker: CodeBreakerImpl = ???
  override def respond(player: ActorRef[Msg], guess: Code): Unit = ???
  override def guess(player: ActorRef[Msg], guess: Code): Unit = ???
}
object AIPlayer {
  def apply(): Behavior[Msg] = new AIPlayer().idle(null)

}
/**
 * Game's corrupted referee
 */
sealed trait Referee {
  var playersTurn: Option[Int]
  var codeLength: Option[Int]
  var sendGuessToOthers: Option[Boolean]
  var players: Option[Map[ActorRef[Msg], Option[Code]]]


  /** Generates player codes
   *
   * @return true if codes have been set, false if they already were set
   * */
  def generateCodes(): Boolean
  /**
   * Check if a player actually won
   * @param value values to check
   */
  def checkWin(value: Map[ActorRef[Msg],Code]): Unit
  /**Allows a player to play his turn*/
  def playTurn(play: ActorRef[Msg]): Future[ActorRef[Msg]]
  /**
   *
   */
  def idle() : Behavior[Msg] = {
    Behaviors.receive{
      case (_,StartGameMsg(newCodeLength, newSendGuessToOthers, newPlayers)) => {
        codeLength = Option(newCodeLength)
        sendGuessToOthers = Option(newSendGuessToOthers)
        players = Option(newPlayers)
        refereeTurn()
      }
    }
  }
  def refereeTurn() : Behavior[Msg] = {
    /*play turns*/
    Behaviors.receive{
      case (_,StopGameMsg()) => {
        idle()
      }
      case (_,AllGuessesMsg(_)) => /*checkWin*/ Behaviors.same
    }
  }
}

class RefereeImpl extends Referee{
  override var playersTurn: Option[Int] = Option.empty
  override var codeLength: Option[Int] = Option.empty
  override var sendGuessToOthers: Option[Boolean] = Option.empty
  override var players: Option[Map[ActorRef[Msg], Option[Code]]] = Option.empty

  /** Generates player codes
   *
   * @return true if codes have been set, false if they already were set
   * */
  override def generateCodes(): Boolean = {
    if(players.isDefined) {
      players = Option(players.get.map(el => el._1 -> Option(new Code())))
      true
    } else false
  }

  def getCurrentPlayer: Option[Int] = playersTurn
  def getAllPlayers: Option[List[ActorRef[Msg]]] = if(players.isDefined) Option(players.get.keys.toList) else Option.empty
  def getEnemies(player: ActorRef[Msg]): Option[List[ActorRef[Msg]]] = if(players.isDefined) Option(players.get.keys.toList.filter(_ != player)) else Option.empty

  /** Increments index of current player
   *
   * @return true if its a new round, false otherwise or if players are not yet defined
   * */
  def goNextTurn(): Boolean = {
    if((playersTurn.isEmpty || playersTurn.get == players.get.size - 1) && players.isDefined) {
      players = Option(Random.shuffle(players.get))
      playersTurn = Option(0)
      true
    }
    else if (players.isDefined) {
      playersTurn = Option(playersTurn.get + 1)
      false
    }
    else false
  }

  /**
   * Check if a player actually won
   *
   * @param value values to check
   */
  override def checkWin(value: Map[ActorRef[Msg], Code]): Unit = ???
  /** Allows a player to play his turn */
  override def playTurn(play: ActorRef[Msg]): Future[ActorRef[Msg]] = ???

}
object Referee{
  def apply(): Behavior[Msg] = new RefereeImpl().idle()
}

object GameController {
  def apply() : Behavior[Msg] = Behaviors.setup(context => new noGameController(context))
}

class noGameController(context: ActorContext[Msg]) extends AbstractBehavior[Msg](context) {
  // No game behavior

  import GameController._

  override def onMessage(msg: Msg): Behavior[Msg] = msg match {
    // Initialize game msg
    case msg: InitializeControllerMsg =>
      val referee: ActorRef[Msg] =  context.spawn(Referee(), "Referee")

      // If Human Player is set, create N - 1 AIPlayers
      var playersList: List[ActorRef[Msg]] = List.tabulate(if(msg.getHuman) msg.getPlayers - 1 else msg.getPlayers)(n => context.spawn(AIPlayer(), "Player" + n))
      if (msg.getHuman) playersList = context.spawn(UserPlayer(), "HumanPlayer") :: playersList

      referee ! StartGameMsg(msg.getLength, msg.getResponses, playersList.map(_ -> Option.empty).toMap)
      GUI.logChat("The game has started")

      new inGameController(context, referee)

    case _ =>
      GUI.logChat("Controller received an unexpected Msg")
      Behaviors.same
  }
}

class inGameController(context: ActorContext[Msg], referee: ActorRef[Msg]) extends AbstractBehavior[Msg](context) {
  // Game running Behavior

  import GameController._

  override def onMessage(msg: Msg): Behavior[Msg] = msg match {
    case msg: StopGameMsg =>
      //Terminate game
      referee ! msg
      GUI.logChat("The game has been stopped")
      new noGameController(context)
    case msg: Msg =>
      logChat(msg)
      Behaviors.same
    case _ =>
      GUI.logChat("Controller received an unexpected type of msg")
      Behaviors.same
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
}