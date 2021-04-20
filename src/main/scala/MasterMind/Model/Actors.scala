package MasterMind.Model
import MasterMind.Utility._
import MasterMind.View.GUI
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, TimerScheduler}
import akka.actor.typed.{ActorRef, Behavior, DispatcherSelector}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.swing.event.ButtonClicked
import scala.util.Success

object Timeout {
  final val timeout = 20.seconds
}

/**
 *  player template with FSA states and referee reference.
 * @tparam T Type of messages to be sent between players
 * @tparam K Type of secret code to be guessed
 */
sealed trait Player[T,K] {
  def idle():Behavior[T]
  def waitTurn():Behavior[T]
  def myTurn():Behavior[T]
  def referee:ActorRef[T]
}

/**
 * Two type of players involved : AI players & human player, they're represented as FSM
 */
abstract class Players extends Player[Msg,Code] {

  def guess(self: ActorContext[Msg]): Unit

  def setupCode(length:Int, opponents: List[ActorRef[Msg]], referee: ActorRef[Msg]): Unit
  def handleResponse(ctx: ActorContext[Msg], response: Option[Response], sender: Option[ActorRef[Msg]], player: Option[ActorRef[Msg]], guess: Option[Code]): Unit

  var referee: ActorRef[Msg]
  var myOpponents: Map[ActorRef[Msg], (Code, Boolean)]
  var myCode: Code

  case class PimpedResponse(response: Response) {
    def isCorrect: Boolean = response.getBlack == myCode.getLength
  }

  implicit def pimpResponse(response: Response): PimpedResponse = PimpedResponse(response)

  /**
   * Player's secret number, evaluated once on first invocation
   * *
   * First state: idle
   */
  override def idle(): Behavior[Msg] = {
    println("Hey I'm an actor and I've been spawned!")
    Behaviors.receive {
      case (ctx, StartGameMsg(codeLength, _, opponents, ref)) =>
        println("I'm actor " + ctx.self + " and someone told me things are 'bout to get serious!")
        setupCode(codeLength, opponents.filter(x => x != ctx.self), ref)
        waitTurn()
      case (ctx, WinCheckMsg(sender, _, code)) =>
        println(ctx.self + " just received a win check msg, sending response!")
        referee ! WinCheckResponseMsg(ctx.self, sender, code, myCode.getResponse(code))
        Behaviors.same
      case (ctx, _: StopGameMsg) => println(ctx.self + " Stopping..."); Behaviors.stopped;
      case _ => Behaviors.same
    }
  }

  /**
   * State in which player's are waiting for their turn
   *
   * @return //
   */
  def waitTurn(): Behavior[Msg] = Behaviors.receive {
    case (ctx, _: YourTurnMsg) =>
      println(ctx.self + " starting a turn!")
      guess(ctx)
      myTurn()
    // Received a guess from another player, send response to referee
    case (ctx, GuessMsg(sender, _, code)) =>
      referee ! GuessResponseMsg(ctx.self, sender, code, myCode.getResponse(code))
      Behaviors.same
    case (ctx, WinCheckMsg(sender, _, code)) =>
      println(ctx.self + " just received a win check msg, sending response!")
      referee ! WinCheckResponseMsg(ctx.self, sender, code, myCode.getResponse(code))
      Behaviors.same
    case (ctx, GuessResponseMsg(sender, player , code , response)) =>
      //SharedResponses MUST be on so process response from other player
      handleResponse(ctx, Option(response), Option(sender), Option(player), Option(code))
      referee ! ReceivedResponseMsg(ctx.self)
      Behaviors.same
    case (ctx, _: VictoryDenyMsg) => println(ctx.self + " failed to win, stopping..."); idle();
    case (ctx, _: StopGameMsg) => println(ctx.self + " Stopping..."); Behaviors.stopped;
    case _ => Behaviors.same
  }

  /**
   * State representing player's turn and actions
   *
   * @return
   *
   */
  def myTurn(): Behavior[Msg] = Behaviors.receive {
    case (ctx, GuessResponseMsg(sender, player , code , response)) =>
      handleResponse(ctx, Option(response), Option(sender), Option(player), Option(code))
      if(ctx.self == player) {
        referee ! ReceivedResponseMsg(ctx.self)
        waitTurn()
      } else {
        Behaviors.same
      }
    case(ctx, _: TurnEnd) =>
      println(ctx.self+" received turn end")
      handleResponse(ctx, Option.empty, Option.empty, Option.empty, Option.empty)
      waitTurn()
    case (ctx, GuessMsg(sender, _, code)) =>
      println(ctx.self + " just received a guess msg, sending response!")
      referee ! GuessResponseMsg(ctx.self, sender, code, myCode.getResponse(code))
      Behaviors.same
    case (ctx, WinCheckMsg(sender, _, code)) =>
      println(ctx.self + " just received a win check msg, sending response!")
      referee ! WinCheckResponseMsg(ctx.self, sender, code, myCode.getResponse(code))
      Behaviors.same
    case (_, _: VictoryDenyMsg) => println("Failed to win, stopping..."); idle();
    case (ctx, _: StopGameMsg) => println(ctx.self + " Stopping..."); Behaviors.stopped;
    case _ => Behaviors.same
  }
}


class UserPlayer extends Players {
  override var myCode: Code = _
  override var myOpponents: Map[ActorRef[Msg],(Code,Boolean)] = Map.empty
  var referee: ActorRef[Msg] = _
  var myLastGuess: Code = _

  override def guess(self:ActorContext[Msg]): Unit = {
    //TODO HANDLE GUI
    var guess: Map[String, Code] = Map.empty
    var click: Int = 0
    println(click)
    GUI.humanPanel.sendGuess.reactions += { case ButtonClicked(_) => guess = Map.empty
      if (click == 0) {
        guess = GUI.humanPanel.getGuess
        val target = myOpponents.filter(x => x._1.path.name.equals(guess.head._1))
        if (target != Map.empty) {
          myLastGuess = guess.head._2
          referee ! GuessMsg(self.self, target.head._1, guess.head._2)
        }
        click = click + 1
      }
    }
  }

  override def setupCode(length: Int, opponents: List[ActorRef[Msg]], ref: ActorRef[Msg]): Unit = {
    myCode = Code(length)
    myOpponents = opponents.map(x => x->(Code(length),false)).toMap
    referee = ref
  }

  override def handleResponse(ctx: ActorContext[Msg], response: Option[Response], sender: Option[ActorRef[Msg]], player: Option[ActorRef[Msg]], guess: Option[Code]): Unit = {
    //TODO VISUALIZE WITH GUI
    if (ctx.self == player.get) {
      if (response.isDefined && response.get.isCorrect) {
        myOpponents = myOpponents.map { el =>
          if (el._1 == sender.get)
            (el._1, (myLastGuess, true))
          else el
        }
        if (myOpponents.values.map(_._2).reduce(_ & _)) {
          referee ! AllGuessesMsg(ctx.self, myOpponents.map(x => x._1 -> x._2._1))
        }
      }
    }
  }
}

object UserPlayer {
  def apply(): Behavior[Msg] = new UserPlayer().idle()
}

class AIPlayer extends Players {
  override var myCode: Code = _
  override var myOpponents:Map[ActorRef[Msg],(Code,Boolean)] = Map.empty
  var sharedGuess: Map[ActorRef[Msg], CodeBreakerImpl] = Map.empty
  var storedGuess: Map[ActorRef[Msg], Option[Code]] = Map.empty
  var referee: ActorRef[Msg] = _
  // Player is already elaborating a response
  var guessing: Boolean = false
  // Player already calculated a guess but timed out
  var timedOut: Boolean = false

  override def guess(ctx: ActorContext[Msg]): Unit = {
    implicit val executionContext: ExecutionContext =
      ctx.system.dispatchers.lookup(DispatcherSelector.fromConfig("my-blocking-dispatcher"))
    // Look for a not already guessed code
    val target = myOpponents.find(x => !x._2._2)

    if (target.isDefined) {
      // Already calculated a code but got timed out
      if(storedGuess(target.get._1).isDefined) {
        referee ! GuessMsg(ctx.self, target.get._1, storedGuess(target.get._1).get)
        storedGuess = storedGuess.map { case (player, stored) => if(player == target.get._1) player -> Option.empty else player -> stored }
      } else if(!guessing) {
        // If not already guessing async calculate a guess
        sharedGuess.find(x => x._1.equals(target.get._1)).get._2.guess.onComplete {
          case Success(_) =>
            val calculatedGuess = sharedGuess.find(x => x._1.equals(target.get._1)).get._2.getGuess
            // If calculated a response in time
            if(!timedOut)
              referee ! GuessMsg(ctx.self, target.get._1, calculatedGuess.get)
            else {
              storedGuess = storedGuess.map { case (player, stored) => if(player == target.get._1) player -> calculatedGuess else player -> stored }
              timedOut = false
            }
            guessing = false
          case _ => println("Something went wrong while computing a guess")
        }
        guessing = true
      }
    } else {
      // If by a weird coincidence i already have all guesses try to win
      referee ! AllGuessesMsg(ctx.self, myOpponents.map(x => x._1 -> x._2._1))
    }
  }

  override def setupCode(length:Int, opponents: List[ActorRef[Msg]], ref: ActorRef[Msg]): Unit = {
    myCode = Code(length)
    sharedGuess = opponents.map(x => x->CodeBreakerImplObj(length, myCode.getRange)).toMap
    storedGuess = opponents.map(x => x->Option.empty).toMap
    myOpponents = opponents.map(x => x->(Code(length),false)).toMap
    referee = ref
  }

  override def handleResponse(ctx: ActorContext[Msg], response: Option[Response], sender: Option[ActorRef[Msg]], player: Option[ActorRef[Msg]], guess: Option[Code]): Unit = {
    // If only ctx is set means its a Timeout Msg
    if(response.isEmpty) {
      timedOut = true
    }
    // If its a response to my guess so its still my turn
    else if (ctx.self == player.get) {
      if (response.isDefined) {
        sharedGuess.find(x => x._1.equals(sender.get)).get._2.receiveKey(response.get)
        // If its correct might try to win
        if (response.get.isCorrect) {
          myOpponents = myOpponents.map { el =>
            if (el._1 == sender.get)
              (el._1, (sharedGuess.find(x => x._1.equals(sender.get)).get._2.lastGuess.get, true))
            else el
          }
          if (myOpponents.values.map(_._2).reduce(_ & _)) {
            referee ! AllGuessesMsg(ctx.self, myOpponents.map(x => x._1 -> x._2._1))
          }
        }
      }
    }
    // SharedResponse
    else if (ctx.self != sender.get && response.isDefined) {
      // If the response is correct we save it else we compute it
      if(response.get.isCorrect) {
        myOpponents = myOpponents.map { el =>
          if (el._1 == sender.get)
            (el._1, (guess.get, true))
          else el
        }
      } else {
        sharedGuess.find(x => x._1.equals(sender.get)).get._2.lastGuess = guess
        sharedGuess.find(x => x._1.equals(sender.get)).get._2.receiveKey(response.get)
      }
    }
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
  def winCheckRoutine(ctx: ActorContext[Msg], winner:ActorRef[T], guesses: Map[ActorRef[T],K]) :Behavior[T]
  def players:Option[List[ActorRef[T]]]
}
/**
 * Game's corrupted referee
 */
abstract class AbstractReferee extends Referee[Msg,Code] {
  val controller: ActorRef[Msg]
  var codeLength: Option[Int]
  var sharedResponse: Boolean
  var currentPlayer: Option[ActorRef[Msg]]
  var players: Option[List[ActorRef[Msg]]]
  var receivedResponses: Int

  case class PimpedResponse(response: Response) {
    def isCorrect: Boolean = if(codeLength.isDefined) response.getBlack == codeLength.get else false
  }

  implicit def pimpResponse(response: Response): PimpedResponse = PimpedResponse(response)

  /** Allows a player to play his turn*/
  def nextPlayerTurn(timers: TimerScheduler[Msg]):Unit

  /** Excludes a player */
  def playerOut(player: ActorRef[Msg]):Unit

  def setupGame(length:Int, players:List[ActorRef[Msg]], sharedResponse: Boolean):Unit
  /**
   *
   */
  override def idle() : Behavior[Msg] = {
    println("Referee has been spawned!")
    Behaviors.receive{
      case (_,StartGameMsg(codeLength, sharedResponse, newPlayers,_)) =>
        println("Ref just received startGameMsg")
        setupGame(codeLength, newPlayers, sharedResponse)
        refereeTurn()
      case (ctx, msg: StopGameMsg) =>
        println(ctx.self + " Stopping...")
        if(players.isDefined) {
          for (player <- players.get) {
            player ! msg
          }
        }
        Behaviors.stopped
      case _ => Behaviors.same
    }
  }

  override def refereeTurn() : Behavior[Msg] = Behaviors.withTimers { timers =>
    nextPlayerTurn(timers)
    Behaviors.receive {
      case (ctx, AllGuessesMsg(winner, guesses)) => winCheckRoutine(ctx, winner, guesses)
      // The referee acts as an intermediary between players
      case (_, msg: GuessMsg) =>
        if (currentPlayer.isDefined && currentPlayer.get == msg.getSender) {
          timers.cancelAll()
          controller ! msg
          msg.getPlayer ! msg
        } else {
          println("Player tried to guess after Timeout")
        }
        Behaviors.same
      case (_, msg: ReceivedResponseMsg) =>
        if(!sharedResponse) {
            if (currentPlayer.isDefined && currentPlayer.get == msg.getSender) {
              nextPlayerTurn(timers)
            } else {
              println(msg.getSender + " confirmed response after Timeout")
            }
        } else if(receivedResponses == players.get.length -1) {
          receivedResponses = 0
          nextPlayerTurn(timers)
        }
        else receivedResponses = receivedResponses + 1
        Behaviors.same
      case (_, msg: TurnEnd) =>
        controller ! msg
        msg.getPlayer ! msg //TODO WHAT?
        nextPlayerTurn(timers)
        Behaviors.same
      case (_, msg: GuessResponseMsg) =>
        controller ! msg
        if(!sharedResponse)
          msg.getPlayer ! msg
        else {
          for(player <- players.get) {
            player ! msg
          }
        }
        Behaviors.same
      case (ctx, msg: StopGameMsg) =>
        println(ctx.self + " Stopping...")
        if(players.isDefined) {
          for (player <- players.get) {
            player ! msg
          }
        }
        Behaviors.stopped
      case (_, msg: Msg) => //TODO WHAT? RIDONDANTE; OTTIMA SOLUZIONE MA TOGLI DALLE ALTRE PARTI
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
   * @param guesses list of other players and their alleged secret code
   * @return //
   */
  override def winCheckRoutine(ctx: ActorContext[Msg], winner:ActorRef[Msg], guesses: Map[ActorRef[Msg], Code]): Behavior[Msg] = {
    if(players.isEmpty) {
      idle()
    }

    var responses = players.get.size - 1
    for(player <- guesses) {
      player._1 ! WinCheckMsg(ctx.self,player._1,player._2)
    }

    Behaviors.receive {
      case (_,WinCheckResponseMsg(_, _, _, response)) =>
        if(response.isCorrect) {
          if(responses - 1 == 0) {
            println(ctx.self + " Stopping...")
            if(players.isDefined) {
              for (player <- players.get) {
                player ! StopGameMsg()
              }
            }
            controller ! VictoryConfirmMsg(winner)
            Behaviors.stopped
          } else {
            responses = responses-1
            Behaviors.same
          }
       } else {
          winner ! VictoryDenyMsg(winner)
          playerOut(winner)
          refereeTurn()
        }
      case (ctx, msg: StopGameMsg) =>
        println(ctx.self + " Stopping...")
        if(players.isDefined) {
          for (player <- players.get) {
            player ! msg
          }
        }
        Behaviors.stopped
      case _ => Behaviors.same
    }
  }
}

class RefereeImpl(private val controllerRef: ActorRef[Msg]) extends AbstractReferee {
  override var codeLength: Option[Int] = Option.empty
  override val controller: ActorRef[Msg] = controllerRef
  override var currentPlayer: Option[ActorRef[Msg]] = Option.empty
  override var players: Option[List[ActorRef[Msg]]] = Option.empty
  override var sharedResponse: Boolean = false
  override var receivedResponses: Int = 0

  var turnManager: TurnManager = TurnManager()

  def getAllPlayers: Option[List[ActorRef[Msg]]] = if(players.isDefined) Option(players.get) else Option.empty
  def getEnemies(player: ActorRef[Msg]): Option[List[ActorRef[Msg]]] = if(players.isDefined) Option(players.get.filter(_ != player)) else Option.empty

  /**
   * Tells to a player to start his turn and sets a timer that defines time in which a player has to make a guess.
   * If such guess isn't made, sends that user an end turn message, fails the promise of his turn and allows next
   * player to play his turn
   */
  override def nextPlayerTurn(timers: TimerScheduler[Msg]): Unit = {
    currentPlayer = Option(turnManager.nextPlayer)
    if(turnManager.index == 0) {
      controller ! TurnOrderMsg(turnManager.players.map(x => x.path.name).toList)
    }
    if(currentPlayer.isDefined) {
      controller ! YourTurnMsg(currentPlayer.get)
      currentPlayer.get ! YourTurnMsg(currentPlayer.get)
      timers.startSingleTimer(TurnEnd(currentPlayer.get),Timeout.timeout)
    }
  }

  override def setupGame(length: Int, newPlayers: List[ActorRef[Msg]], shared: Boolean): Unit = {
    codeLength = Option(length)
    sharedResponse = shared
    players = Option(newPlayers)
    turnManager.setPlayers(newPlayers)
  }

  override def playerOut(player: ActorRef[Msg]): Unit = {
    players = Option(players.get.filter(el => el != player))
    turnManager.removePlayer(player)
    println(player + " failed to win, removing him, now players size = " + players.size)
  }
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
        Behaviors.stopped
      } else {
        GUI.logChat("Critical error: Referee Not initialized")
        Behaviors.same
      }
    case (_, msg: VictoryConfirmMsg) =>
      logChat(msg)
      GUI.logChat("The game has been stopped")
      Behaviors.stopped
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
    case msg : TurnOrderMsg =>  GUI.logChat("The order for this round is " + msg.getTurns.mkString(" -> ") + "\n")
    case msg : YourTurnMsg => GUI.logChat(msg.getPlayer.path.name + " it's your turn!")
    case msg : GuessMsg => GUI.logChat(msg.getSender.path.name + " trying to guess " + msg.getPlayer.path.name + " code -> " + msg.getGuess)
    case msg : AllGuessesMsg => GUI.logChat(msg.getPlayer.path.name + " is trying to guess all codes:\n" + msg.getGuesses.map(x => x._1 + " -> " + x._2).mkString(",\n"))
    case msg : GuessResponseMsg => GUI.logChat("Response from " + msg.getSender.path.name + " to " + msg.getPlayer.path.name + " -> " + msg.getResponse + "\n")
    case msg : TurnEnd => GUI.logChat(msg.getPlayer.path.name + " did not answer in time\n")
    case msg : VictoryConfirmMsg => GUI.logChat(msg.getPlayer.path.name + " just won the game! YAY!")
    case msg : VictoryDenyMsg => GUI.logChat(msg.getPlayer.path.name + " failed miserably his attempt at winning the game.")
    case _ => GUI.logChat("Controller received an unexpected Msg")
  }
}

object GameController {
  def apply() : Behavior[Msg] = new GameController().noGameBehavior()
}