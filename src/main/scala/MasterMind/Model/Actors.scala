package MasterMind.Model
import MasterMind.Utility._
import MasterMind.View.GUI
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, TimerScheduler}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}

import scala.concurrent.duration._
import scala.swing.event.ButtonClicked

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
  def waitTurn():Behavior[T]
  def myTurn():Behavior[T]
  def referee:ActorRef[T]
}

/**
 * Two type of players involved : AI players & human player, they're represented as FSM
 */
abstract class Players extends Player[Msg,Code] {

  def guess(self: ActorContext[Msg]): Unit

  def setupCode(opponents: List[ActorRef[Msg]], referee: ActorRef[Msg]): Unit
  def handleResponse(ctx: ActorContext[Msg], response: Option[Response], sender: Option[ActorRef[Msg]]): Unit

  var referee: ActorRef[Msg]
  var myOpponents: Map[ActorRef[Msg], (Code, Boolean)]
  var myCode: Code
  var codeBreaker: CodeBreakerImpl

  /**
   * Player's secret number, evaluated once on first invocation
   * *
   * First state: idle
   */
  override def idle(): Behavior[Msg] = {
    println("Hey I'm an actor and I've been spawned!")
    Behaviors.receive {
      case (ctx, StartGameMsg(_, opponents, ref)) =>
        println("I'm actor " + ctx.self + " and someone told me things are 'bout to get serious!")
        setupCode(opponents.filter(x => x != ctx.self), ref)
        waitTurn()
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
      println(ctx.self + " just received a guess msg, sending response!")
      referee ! GuessResponseMsg(ctx.self, sender, code, myCode.getResponse(code))
      Behaviors.same
    case (ctx, _: StopGameMsg) => println(ctx.self + " Stopping..."); Behaviors.stopped;
    case _ => Behaviors.same
  }

  /**
   * State representing player's turn and actions
   *
   * @return
   */
  def myTurn(): Behavior[Msg] = Behaviors.receive {
    case (ctx, GuessResponseMsg(sender, _ , _ , response)) =>
      referee ! ReceivedResponseMsg(ctx.self)
      handleResponse(ctx, Option(response), Option(sender))
      waitTurn()
    case(ctx, _: TurnEnd) =>
      handleResponse(ctx, Option.empty, Option.empty)
      waitTurn()
    case (ctx, GuessMsg(sender, _, code)) =>
      println(ctx.self + " just received a guess msg, sending response!")
      referee ! GuessResponseMsg(ctx.self, sender, code, myCode.getResponse(code))
      Behaviors.same
    case (ctx, _: StopGameMsg) => println(ctx.self + " Stopping..."); Behaviors.stopped;
    case _ => Behaviors.same
  }
}


class UserPlayer extends Players {
  override var myCode: Code = Code()
  override var codeBreaker: CodeBreakerImpl = CodeBreakerImplObj(myCode.getRange)
  override var myOpponents:Map[ActorRef[Msg],(Code,Boolean)] = Map.empty
  var referee: ActorRef[Msg] = _

  override def guess(self:ActorContext[Msg]): Unit = {
    //TODO HANDLE GUI
    var guess: Map[String, Code] = Map.empty
    GUI.humanPanel.sendGuess.reactions += { case ButtonClicked(_) => guess = Map.empty
      guess = GUI.humanPanel.getGuess
      println(guess)
      val target = myOpponents.filter(x => x._1.path.name == guess.head._1)
      if(target != Map.empty) {
        codeBreaker.lastGuess = guess.head._2
        referee ! GuessMsg(self.self, target.head._1, guess.head._2)
      }
    }
  }

  override def setupCode(opponents:List[ActorRef[Msg]],ref:ActorRef[Msg]): Unit = {
    myOpponents = opponents.map(x => x->(Code(),false)).toMap
    referee = ref
  }

  override def handleResponse(ctx: ActorContext[Msg], response: Option[Response], sender: Option[ActorRef[Msg]]): Unit = {
    //TODO VISUALIZE WITH GUI
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

  override def guess(ctx: ActorContext[Msg]): Unit = {
    val target = myOpponents.find(x => !x._2._2)
    if (target.isDefined) {
      if(storedGuess.isDefined) {
        referee ! GuessMsg(ctx.self, target.get._1, storedGuess.get)
        storedGuess = Option.empty
      } else
        referee ! GuessMsg(ctx.self, target.get._1, codeBreaker.guess)
    } else {
      referee ! AllGuessesMsg(ctx.self,myOpponents map { case (actor, code -> _) => actor->code })
    }
  }

  override def setupCode(opponents:List[ActorRef[Msg]],ref:ActorRef[Msg]): Unit = {
    myOpponents = opponents.map(x => x->(Code(),false)).toMap
    referee = ref
  }

  override def handleResponse(ctx: ActorContext[Msg], response: Option[Response], sender: Option[ActorRef[Msg]]): Unit = {
    if(response.isDefined) {
      codeBreaker.receiveKey(response.get)
      if (response.get.isCorrect) {
        myOpponents = myOpponents.map { el =>
          if (el._1 == sender.get)
            (el._1, (codeBreaker.lastGuess, true))
          else el
        }
        if (myOpponents.values.map(_._2).reduce(_ & _)) {
          referee ! AllGuessesMsg(ctx.self, myOpponents.map(x => x._1 -> x._2._1))
        } else {
          codeBreaker = CodeBreakerImplObj(myCode.getRange) //TODO check if such change is reflected into myOpponents
        }
      }
    } else {
      storedGuess = Option(codeBreaker.lastGuess)
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
  var currentPlayer: Option[ActorRef[Msg]]
  var players: Option[List[ActorRef[Msg]]]

  /** Allows a player to play his turn*/
  def nextPlayerTurn(timers: TimerScheduler[Msg]):Unit

  /** Excludes a player */
  def playerOut(player: ActorRef[Msg]):Unit

  def setupGame(players:List[ActorRef[Msg]]):Unit
  /**
   *
   */
  override def idle() : Behavior[Msg] = {
    println("Referee has been spawned!")
    Behaviors.receive{
      case (_,StartGameMsg(_, newPlayers,_)) =>
        println("Ref just received startGameMsg")
        setupGame(newPlayers)
        refereeTurn()
      case (ctx, msg: StopGameMsg) => {
        println(ctx.self + " Stopping...")
        if(players.isDefined) {
          for (player <- players.get) {
            player ! msg
          }
        }
        Behaviors.stopped
      }
      case _ => Behaviors.same
    }
  }

  override def refereeTurn() : Behavior[Msg] = Behaviors.withTimers { timers =>
    nextPlayerTurn(timers)
    Behaviors.receive {
      case (ctx, AllGuessesMsg(winner, guesses)) => winCheckRoutine(ctx, winner, guesses)
      // The referee acts as an intermediary between players
      case (_, msg: GuessMsg) =>
        //TODO IMPLEMENT ANSWER TO ALL PLAYERS
        if (currentPlayer.isDefined && currentPlayer.get == msg.getSender) {
          timers.cancelAll()
          controller ! msg
          msg.getPlayer ! msg
        } else {
          println("Player tried to guess after Timeout")
        }
        Behaviors.same
      case (_, msg: ReceivedResponseMsg) =>
        if (currentPlayer.isDefined && currentPlayer.get == msg.getSender) {
          nextPlayerTurn(timers)
        } else {
          println("Player confirmed response after Timeout")
        }
        Behaviors.same
      case (_, msg: TurnEnd) =>
        controller ! msg
        msg.getPlayer ! msg
        nextPlayerTurn(timers)
        Behaviors.same
      case (_, msg: GuessResponseMsg) =>
        controller ! msg
        msg.getPlayer ! msg
        Behaviors.same
      case (ctx, msg: StopGameMsg) => {
        println(ctx.self + " Stopping...")
        if(players.isDefined) {
          for (player <- players.get) {
            player ! msg
          }
        }
        Behaviors.stopped
      }
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
   * @param guesses list of other players and their alleged secret code
   * @return //
   */
  override def winCheckRoutine(ctx: ActorContext[Msg], winner:ActorRef[Msg], guesses: Map[ActorRef[Msg], Code]): Behavior[Msg] = {
    if(players.isEmpty) {
      idle()
    }
    var responses = players.get.size - 1
    for(player <- guesses) {
      player._1 ! GuessMsg(ctx.self,player._1,player._2)
    }

    Behaviors.receive {
      case (_,GuessResponseMsg(_, _, _, response)) =>
        if(response.isCorrect) {
          if(responses - 1 == 0) {
            controller ! VictoryConfirmMsg(winner)
            for(player <- players.get) {
              player ! StopGameMsg()
            }
            idle()
          } else {
            responses = responses-1
            Behaviors.same
          }
       } else {
          winner ! VictoryDenyMsg(winner)
          playerOut(winner)
          refereeTurn()
        }
      case (ctx, msg: StopGameMsg) => {
        println(ctx.self + " Stopping...")
        if(players.isDefined) {
          for (player <- players.get) {
            player ! msg
          }
        }
        Behaviors.stopped
      }
      case _ => println("Something went bad during win check routine,initializing it again"); winCheckRoutine(ctx, winner, guesses)
    }
  }
}

class RefereeImpl(private val controllerRef: ActorRef[Msg]) extends AbstractReferee {
   override val controller: ActorRef[Msg] = controllerRef
   override var currentPlayer: Option[ActorRef[Msg]] = Option.empty
   override var players: Option[List[ActorRef[Msg]]] = Option.empty

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
    if(currentPlayer.isDefined) {
      controller ! YourTurnMsg(currentPlayer.get)
      currentPlayer.get ! YourTurnMsg(currentPlayer.get)
      timers.startSingleTimer(TurnEnd(currentPlayer.get),Timeout.timeout)
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
        Behaviors.stopped
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
    case msg : GuessResponseMsg => GUI.logChat("Response from " + msg.getSender.path.name + " to " + msg.getPlayer.path.name + " -> " + msg.getResponse + "\n")
    case msg : TurnEnd => GUI.logChat("Player " + msg.getPlayer.path.name + " did not answer in time")
    case msg : VictoryConfirmMsg => GUI.logChat(msg.getPlayer.path.name + " just won the game! YAY!")
    case msg : VictoryDenyMsg => GUI.logChat(msg.getPlayer.path.name + " failed miserably his attempt at winning the game.")
    case _ => GUI.logChat("Controller received an unexpected Msg")
  }
}

object GameController {
  def apply() : Behavior[Msg] = new GameController().noGameBehavior()
}