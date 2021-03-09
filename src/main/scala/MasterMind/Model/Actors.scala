package MasterMind.Model
import MasterMind.Utility._
import MasterMind.View.GUI
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise, TimeoutException}
import scala.util.{Failure, Success}

object Timeout {
  final val timeout = 10
}

/**
 *  player template with FSA states and referee reference.
 * @tparam T Type of messages to be sent between players
 * @tparam K Type of secret code to be guessed
 */
sealed trait Player[T,K] {
  def idle():Behavior[T]
  def waitTurn(mySecretNumber:K):Behavior[T]
  def myTurn(mySecretNumber:K):Behavior[T]
  def referee:ActorRef[T]
}

/**
 * Two type of players involved : AI players & human player, they're represented as FSM
 */
abstract class Players extends Player[Msg,Code]{

  def respond(self:ActorRef[Msg],player:ActorRef[Msg] ,guess:Code): Unit
  def guess(self:ActorRef[Msg]): Unit
  def setupCode(codeLength:Int,opponents: List[ActorRef[Msg]],referee:ActorRef[Msg]):Unit

  var referee:ActorRef[Msg]
  var myOpponents:Map[ActorRef[Msg],(Code,Boolean)]
  var myCode: Code
  var codeBreaker: CodeBreakerImpl
  /**
   * Player's secret number, evaluated once on first invocation
    **
     * First state: idle
     */
  override def idle() : Behavior[Msg] = {
    println("Hey I'm an actor and I've been spawned!")
    Behaviors.receive{
      case (ctx,StartGameMsg(codeLength,_,opponents,ref)) =>
        setupCode(codeLength, opponents.filter(x => x != ctx.self), ref)
        waitTurn(myCode);
      case _ => Behaviors.same
    }

  }

  /**
   * State in which player's are waiting for their turn
   * @param mySecretNumber my secret number
   * @return //
   */
   def waitTurn(mySecretNumber:Code): Behavior[Msg] = Behaviors.receive{
      case (_,_ : YourTurnMsg) =>  myTurn(mySecretNumber)
      case (ctx, GuessMsg(actor,code)) => respond(ctx.self,actor,code); Behaviors.same
      case _ => Behaviors.same
  }

  /**
   * State representing player's turn and actions
   * @param mySecretNumber my secret number
   * @return
   */
  def myTurn(mySecretNumber:Code): Behavior[Msg] = Behaviors.setup { ctx =>
      guess(ctx.self)
      Behaviors.receive {
        case (ctx, GuessResponseMsg(player, _, response)) => if(response.isCorrect){
          if(myOpponents.values.map(_._2).reduce(_&_)){
            referee ! AllGuessesMsg(ctx.self,myOpponents.map(x=> x._1 -> x._2._1))
          }else{
            codeBreaker = CodeBreakerImplObj()
            myOpponents.find(_._1 equals player).map(player => player._2._2) //TODO check if such change is reflected into myOpponents
          }
        }else{
          codeBreaker.receiveKey(response)
        }; waitTurn(mySecretNumber)
        case (_, _: VictoryConfirmMsg) => println("I WON!"); idle(); //TODO
        case _ => Behaviors.same
      }
    }
}

class UserPlayer extends Players {
  override var myCode: Code = _
  override var codeBreaker: CodeBreakerImpl = _
  override var myOpponents:Map[ActorRef[Msg],(Code,Boolean)] = Map.empty
   var referee: ActorRef[Msg] = _

  override def respond(self:ActorRef[Msg],player: ActorRef[Msg], guess: Code): Unit = {
    //TODO HANDLE GUI
  }
  override def guess(self:ActorRef[Msg]): Unit = {
    //TODO HANDLE GUI
  }

  override def setupCode(codeLength: Int, opponents:List[ActorRef[Msg]],ref:ActorRef[Msg]): Unit = {
    myCode = Code(codeLength)
    codeBreaker = CodeBreakerImplObj()
    myOpponents = opponents.map(x => x->(Code(),false)).toMap
    referee = ref
  }
}

object UserPlayer {
  def apply(): Behavior[Msg] = new UserPlayer().idle()
}

class AIPlayer extends Players {
  override var myCode: Code = _
  override var codeBreaker: CodeBreakerImpl = _
  override var myOpponents:Map[ActorRef[Msg],(Code,Boolean)] = Map.empty
   var referee: ActorRef[Msg] = _

  override def respond(self:ActorRef[Msg],player: ActorRef[Msg], guess: Code): Unit = player ! GuessResponseMsg(self,guess,myCode.getResponse(guess))

  override def guess(self: ActorRef[Msg]): Unit = {
    val target = myOpponents.find(x=> !x._2._2)
    if (target.isDefined){
      target.get._1 ! GuessMsg(self,codeBreaker.guess)
    }else{
      referee ! AllGuessesMsg(self,myOpponents map  { case (actor, code -> _) => actor->code })
    }
  }

  override def setupCode(codeLength: Int, opponents:List[ActorRef[Msg]],ref:ActorRef[Msg]): Unit = {
    myCode = Code(codeLength)
    codeBreaker = CodeBreakerImplObj()
    myOpponents = opponents.map(x => x->(Code(),false)).toMap
    referee = ref
  }

}
object AIPlayer {
  def apply(): Behavior[Msg] = new AIPlayer().idle()
}
sealed trait Referee[T,K]{
  def idle():Behavior[T]
  def refereeTurn():Behavior[T]
  def winCheckRoutine(winner:ActorRef[T],players: Map[ActorRef[T],K]) :Behavior[T]
  def players:Option[Map[ActorRef[T],Option[K]]]
}
/**
 * Game's corrupted referee
 */
abstract class AbstractReferee extends Referee[Msg,Code] {

  /** Generates player codes
   *
   * @return true if codes have been set, false if they already were set
   * */
  def generateCodes(): Boolean
  /**Allows a player to play his turn*/
  def nextPlayerTurn():Unit
  def playerOut(player: ActorRef[Msg]):Unit
  var players:Option[Map[ActorRef[Msg],Option[Code]]]

  def playerTurnEnd():Unit
  def setupGame(codeLength:Int,players:List[ActorRef[Msg]]):Unit
  /**
   *
   */
  override def idle() : Behavior[Msg] = {
    println("Referee has been spawned!")
    Behaviors.receive{
      case (_,StartGameMsg(newCodeLength, _, newPlayers,_)) =>
        setupGame(newCodeLength,newPlayers)
        refereeTurn()
      case _ => Behaviors.same
    }
  }

  override def refereeTurn() : Behavior[Msg] = {
      nextPlayerTurn()
      Behaviors.receive{
        case (_,StopGameMsg()) => idle()
        case (_,AllGuessesMsg(winner,guesses)) => winCheckRoutine(winner,guesses)
        case _ => Behaviors.same
      }
    }

  /**
   *  Checks if a player has won by interacting with any other player asking them their secret code (via "Ask Pattern"
   *  https://doc.akka.io/docs/akka/current/typed/interaction-patterns.html#request-response-with-ask-from-outside-an-actor
   *  )
   * @param winner actor who claims to have won
   * @param players list of other players and their alleged secret code
   * @return //
   */
  override def winCheckRoutine(winner:ActorRef[Msg],players: Map[ActorRef[Msg], Code]): Behavior[Msg] = Behaviors.setup{ ctx =>
    implicit val timeout: Timeout = 10.seconds
    case class AdaptedResponse(message: String) extends Msg


    ctx.ask[Msg,Msg](players.keySet.head, ref => GuessMsg(ref,players.values.head)){
      case Success(c: GuessResponseMsg) => c
      case Failure(_) => AdaptedResponse("Failure")
    }

    Behaviors.receive{
      case (_,GuessResponseMsg(player,_,response)) =>
        if(response.isCorrect){
          if(players.size == 1){
            winner ! VictoryConfirmMsg(winner)
            idle()
          }else{
            winCheckRoutine(winner,players-player)
          }
       }else{
          winner ! VictoryDenyMsg(winner)
          playerOut(winner)
          refereeTurn()
        }
      case (_,AdaptedResponse(_)) => println("Something went bad during win check routine,initializing it again");winCheckRoutine(winner,players)
    }
  }
}

class RefereeImpl extends AbstractReferee{
   var codeLength: Option[Int]= Option.empty
   var players: Option[Map[ActorRef[Msg], Option[Code]]]= Option.empty

   var currentPlayerTurn: Future[Unit] = _
   var currentPlayerPromise: Promise[Unit] = _

   var turnManager: TurnManager = TurnManager()



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

  def getAllPlayers: Option[List[ActorRef[Msg]]] = if(players.isDefined) Option(players.get.keys.toList) else Option.empty
  def getEnemies(player: ActorRef[Msg]): Option[List[ActorRef[Msg]]] = if(players.isDefined) Option(players.get.keys.toList.filter(_ != player)) else Option.empty


  /**
   * Tells to a player to start his turn and sets a timer that defines time in which a player has to make a guess.
   * If such guess isn't made, sends that user an end turn message, fails the promise of his turn and allows next
   * player to play his turn
   */
  override def nextPlayerTurn(): Unit = {
    val player  = futurePlayerTurn()
    player ! YourTurnMsg(player)
    try {
      import scala.concurrent.duration._
      Await.result(currentPlayerTurn, Timeout.timeout.seconds)
    } catch {
      case _: TimeoutException => player ! TurnEnd(player); currentPlayerPromise.failure(_); refereeTurn()
    }
  }


  /**
   * tells a player to start his turn and sets two variables to check if he made a guessor if
   * his time and turn are over(@currentPlayer)
   * @return
   */
  def futurePlayerTurn() :ActorRef[Msg]= {
    currentPlayerPromise = Promise[Unit]()
    currentPlayerTurn=currentPlayerPromise.future
    turnManager.nextPlayer
  }

  override def setupGame(cLength: Int,newPlayers: List[ActorRef[Msg]]): Unit = {
    codeLength = Option(cLength)
    players = Option(newPlayers.map(x => x->Option(Code())).toMap)
    turnManager.setPlayers(newPlayers)
  }

  override def playerOut(player: ActorRef[Msg]): Unit = {players = Some(players.get-player); println("Player failed to win, removing him, now players size = "+players.size); turnManager.removePlayer(player)}

  override def playerTurnEnd(): Unit = currentPlayerPromise.success(()=>())
}
object Referee{
  def apply(): Behavior[Msg] = new RefereeImpl().idle()
}

class GameController {
  var referee: Option[ActorRef[Msg]] = None
  println("Initialized controller") //TODO JUST DEBUG MSG

  // No game behavior
  def noGameBehavior(): Behavior[Msg] = Behaviors.receive {
    // Initialize game msg
    case (context, msg: InitializeControllerMsg) =>
      println("Received Init Msg") //TODO JUST DEBUG MSG

      referee = Some(context.spawn(Referee(), "Referee"))//TODO CHECK

      println("Referee ok") //TODO JUST DEBUG MSG

      // If Human Player is set, create N - 1 AIPlayers
      var playersList: List[ActorRef[Msg]] = List.tabulate(if(msg.getHuman) msg.getPlayers - 1 else msg.getPlayers)(n => context.spawn(AIPlayer(), "Player" + n))
      if (msg.getHuman) playersList = context.spawn(UserPlayer(), "HumanPlayer") :: playersList

      println("Created players: " + playersList) //TODO JUST DEBUG MSG

      referee.get ! StartGameMsg(msg.getLength, msg.getResponses, playersList, referee.get)
      GUI.logChat("The game has started")

      inGameBehavior()

    case _ =>
      GUI.logChat("Controller received an unexpected Msg")
      Behaviors.same
  }

  // Game running Behavior
  def inGameBehavior(): Behavior[Msg] = Behaviors.receive {
    case (_, msg: StopGameMsg) =>
      //Terminate game
      if(referee.isDefined) {
        referee.get ! msg
        GUI.logChat("The game has been stopped")
        noGameBehavior()
      } else {
        GUI.logChat("Critical error: Referee Not initialized")
        Behaviors.same
      }
    case (_, msg: Msg) =>
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

object GameController {
  def apply() : Behavior[Msg] = new GameController().noGameBehavior()
}