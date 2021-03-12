package MasterMind.Model
import MasterMind.Utility._
import MasterMind.View.GUI
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import akka.util.Timeout

import scala.concurrent.duration._
import scala.util.{Failure, Success}

object Timeout {
  final val timeout = 10.seconds
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

  def guess(self:ActorRef[Msg]): Unit
  def setupCode(opponents: List[ActorRef[Msg]], referee:ActorRef[Msg]):Unit

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
      case (ctx,StartGameMsg(_,opponents,ref)) =>
        println("I'm actor "+ctx.self+" and someone told me things are 'bout to get serious!")
        setupCode(opponents.filter(x => x != ctx.self), ref)
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
      case (cx, _ : YourTurnMsg) => println(cx.self +" starting a turn!"); myTurn(mySecretNumber)
        // Received a guess from another player, send response to referee
      case (ctx, GuessMsg(sender, _ , code)) =>
        println(ctx.self +" just received a guess msg, sending response!")
        referee ! GuessResponseMsg(ctx.self, sender, code, myCode.getResponse(code))
        Behaviors.same
      case (cx, _ : StopGameMsg) => println(cx.self +" Stopping..."); idle();
      case _ => Behaviors.same
  }

  /**
   * State representing player's turn and actions
   * @param mySecretNumber my secret number
   * @return
   */
  def myTurn(mySecretNumber:Code): Behavior[Msg] = Behaviors.setup { ctx =>
    println(ctx.self+": just started my turn bitches")
      guess(ctx.self)
      Behaviors.receive {
        case (ctx, GuessResponseMsg(sender, _, _, response)) =>
          if(response.isCorrect) {
            if(myOpponents.values.map(_._2).reduce(_&_)) {
              referee ! AllGuessesMsg(ctx.self,myOpponents.map(x=> x._1 -> x._2._1))
            } else {
              codeBreaker = CodeBreakerImplObj(myCode.getRange)
              myOpponents.find(_._1 equals sender).map(player => player._2._2) //TODO check if such change is reflected into myOpponents
            }
          } else {
            codeBreaker.receiveKey(response)
          }
          waitTurn(mySecretNumber)
        case (_, _ : VictoryConfirmMsg) =>
          println(ctx.self+": told ya I was gonna win this")
          idle(); //TODO
        case (cx, _ : StopGameMsg) => println(cx.self +" Stopping..."); idle();
        case _ => Behaviors.same
      }
    }
}

class UserPlayer extends Players {
  override var myCode: Code = Code()
  override var codeBreaker: CodeBreakerImpl = CodeBreakerImplObj(myCode.getRange)
  override var myOpponents:Map[ActorRef[Msg],(Code,Boolean)] = Map.empty
  var referee: ActorRef[Msg] = _

  override def guess(self:ActorRef[Msg]): Unit = {
    //TODO HANDLE GUI
  }

  override def setupCode(opponents:List[ActorRef[Msg]],ref:ActorRef[Msg]): Unit = {
    myOpponents = opponents.map(x => x->(Code(),false)).toMap
    referee = ref
  }
}

object UserPlayer {
  def apply(): Behavior[Msg] = new UserPlayer().idle()
}

class AIPlayer extends Players {
  override var myCode: Code = Code()
  override var codeBreaker: CodeBreakerImpl = CodeBreakerImplObj(myCode.getRange)
  override var myOpponents:Map[ActorRef[Msg],(Code,Boolean)] = Map.empty
  var storedGuess: Option[Code] = Option.empty
  var referee: ActorRef[Msg] = _

  override def guess(self: ActorRef[Msg]): Unit = {
    val target = myOpponents.find(x => !x._2._2)
    if (target.isDefined) {
      val codes = codeBreaker.guess
      println(codes + "\n" + self)
      referee ! GuessMsg(self, target.get._1, codeBreaker.guess)
    } else {
      referee ! AllGuessesMsg(self,myOpponents map { case (actor, code -> _) => actor->code })
    }
  }

  override def setupCode(opponents:List[ActorRef[Msg]],ref:ActorRef[Msg]): Unit = {
    myOpponents = opponents.map(x => x->(Code(),false)).toMap
    referee = ref
  }

}
object AIPlayer {
  def apply(): Behavior[Msg] = new AIPlayer().idle()
}
sealed trait Referee[T,K]{
  /**
   * waits for game to start
   * @return
   */
  def idle():Behavior[T]
  def refereeTurn():Behavior[T]
  def winCheckRoutine(winner:ActorRef[T],players: Map[ActorRef[T],K]) :Behavior[T]
  def players:Option[List[ActorRef[T]]]
}
/**
 * Game's corrupted referee
 */
abstract class AbstractReferee extends Referee[Msg,Code] {

  val controller: ActorRef[Msg]
  var currentPlayer: Option[ActorRef[Msg]]

  /**Allows a player to play his turn*/
  def nextPlayerTurn(ctx: ActorContext[Msg]):Unit
  def playerOut(player: ActorRef[Msg]):Unit

  /**
   * players in game with their secret code
   */
  var players:Option[List[ActorRef[Msg]]]

  def setupGame(players:List[ActorRef[Msg]]):Unit
  /**
   *
   */
  override def idle() : Behavior[Msg] = {
    println("Referee has been spawned!")
    Behaviors.receive{
      case (ctx,StartGameMsg(_, newPlayers,_)) =>
        println("Ref just received startGameMsg")
        setupGame(newPlayers)
        refereeTurn()
      case _ => Behaviors.same
    }
  }

  override def refereeTurn() : Behavior[Msg] = Behaviors.setup{ ctx =>
      println("Ref arbitrando next turn")
      nextPlayerTurn(ctx)
      Behaviors.receive{
        case (_,StopGameMsg()) => idle()
        case (_,AllGuessesMsg(winner,guesses)) => winCheckRoutine(winner,guesses)
          // The referee acts as an intermediary between players
        case (context,msg: GuessMsg) =>
          //TODO IMPLEMENT ANSWER TO ALL PLAYERS
          if(currentPlayer.isDefined && currentPlayer.get == msg.getSender) {
            controller ! msg
            msg.getPlayer ! msg
            nextPlayerTurn(context)
          } else {
            println("Player tried to guess after Timeout")
          }
          Behaviors.same
        case (context,msg: TurnEnd) =>
          controller ! msg
          msg.getPlayer ! msg
          nextPlayerTurn(context)
          Behaviors.same
        case (_,msg: GuessResponseMsg) =>
          controller ! msg
          msg.getPlayer ! msg
          Behaviors.same
        case (_, msg: Msg) =>
          controller ! msg
          Behaviors.same
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
    implicit val timeout: Timeout = Timeout.timeout
    case class AdaptedResponse(message: String) extends Msg

    ctx.ask[Msg,Msg](players.keySet.head, ref => GuessMsg(ref, players.keySet.head, players.values.head)){
      case Success(c: GuessResponseMsg) => c
      case _ => AdaptedResponse("Failure")
    }

    Behaviors.receive {
      case (ctx,GuessResponseMsg(_, player, _, response)) =>
        if(response.isCorrect) {
          if(players.size == 1) {
            winner ! VictoryConfirmMsg(winner)
            println(winner +" just incredibly won!")
            idle()
          } else {
            winCheckRoutine(winner,players-player)
          }
       } else {
          winner ! VictoryDenyMsg(winner)
          playerOut(winner)
          refereeTurn()
        }
      case _ => println("Something went bad during win check routine,initializing it again"); winCheckRoutine(winner,players)
    }
  }
}

class RefereeImpl(private val controllerRef: ActorRef[Msg]) extends AbstractReferee{
   override val controller: ActorRef[Msg] = controllerRef
   override var currentPlayer: Option[ActorRef[Msg]] = Option.empty
   var players: Option[List[ActorRef[Msg]]] = Option.empty

   var turnManager: TurnManager = TurnManager()

  def getAllPlayers: Option[List[ActorRef[Msg]]] = if(players.isDefined) Option(players.get) else Option.empty
  def getEnemies(player: ActorRef[Msg]): Option[List[ActorRef[Msg]]] = if(players.isDefined) Option(players.get.filter(_ != player)) else Option.empty

  /**
   * Tells to a player to start his turn and sets a timer that defines time in which a player has to make a guess.
   * If such guess isn't made, sends that user an end turn message, fails the promise of his turn and allows next
   * player to play his turn
   */
  override def nextPlayerTurn(ctx: ActorContext[Msg]): Unit = {
    implicit val timeout: Timeout = Timeout.timeout
    currentPlayer = Option(turnManager.nextPlayer)

    ctx.ask[Msg,Msg](currentPlayer.get, ref => YourTurnMsg(ref)) {
      case Success(msg: GuessMsg) => msg
      case Failure(_) => println(currentPlayer.get +" didn't guess in time23"); TurnEnd(currentPlayer.get)
      case _ => TurnEnd(currentPlayer.get)
    }
  }

  override def setupGame(newPlayers: List[ActorRef[Msg]]): Unit = {
    players = Option(newPlayers) //TODO FOR EACH PLAYER GET CODE
    turnManager.setPlayers(newPlayers)
  }

  override def playerOut(player: ActorRef[Msg]): Unit = {players = Some(players.get.filter(el => el != player)); println("Player failed to win, removing him, now players size = "+players.size); turnManager.removePlayer(player)}
}
object Referee{
  def apply(controller: ActorRef[Msg]): Behavior[Msg] = new RefereeImpl(controller).idle()
}

class GameController {
  var referee: Option[ActorRef[Msg]] = None
  println("Initialized controller") //TODO JUST DEBUG MSG

  // No game behavior
  def noGameBehavior(): Behavior[Msg] = Behaviors.receive {
    // Initialize game msg
    case (context, msg: InitializeControllerMsg) =>
      println("Received Init Msg") //TODO JUST DEBUG MSG
      Code.setLength(msg.getLength)

      referee = Some(context.spawn(Referee(context.self), "Referee"))//TODO CHECK

      println("Referee ok") //TODO JUST DEBUG MSG

      // If Human Player is set, create N - 1 AIPlayers
      var playersList: List[ActorRef[Msg]] = List.tabulate(if(msg.getHuman) msg.getPlayers - 1 else msg.getPlayers)(n => context.spawn(AIPlayer(), "Player" + n))
      if (msg.getHuman) playersList = context.spawn(UserPlayer(), "HumanPlayer") :: playersList

      println("Created players: " + playersList) //TODO JUST DEBUG MSG

      playersList foreach (_ ! StartGameMsg(msg.getLength, msg.getResponses, playersList, referee.get))
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
    case msg : YourTurnMsg => GUI.logChat(msg.getPlayer.path.name + " it's your turn!")
    case msg : GuessMsg => GUI.logChat(msg.getSender.path.name + " trying to guess " + msg.getPlayer.path.name + " code -> " + msg.getGuess)
    case msg : AllGuessesMsg => GUI.logChat(msg.getPlayer.path.name + " is trying to guess all codes:\n" + msg.getGuesses.map(x => x._1 + " -> " + x._2).mkString(",\n"))
    case msg : GuessResponseMsg => GUI.logChat("Response from " + msg.getSender.path.name + " to " + msg.getPlayer.path.name + " -> " + msg.getResponse)
    case msg : TurnEnd => GUI.logChat("Player " + msg.getPlayer.path.name + " did not answer in time")
    case msg : VictoryConfirmMsg => GUI.logChat(msg.getPlayer.path.name + " just won the game! YAY!")
    case msg : VictoryDenyMsg => GUI.logChat(msg.getPlayer.path.name + " failed miserably his attempt at winning the game.")
    case _ => GUI.logChat("Controller received an unexpected Msg")
  }
}

object GameController {
  def apply() : Behavior[Msg] = new GameController().noGameBehavior()
}