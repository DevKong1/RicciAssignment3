package MasterMind.Utility

import akka.actor.typed.ActorRef

 trait Msg {
}

//
//  Player Messages
//

case class GuessMsg(private val player: ActorRef[Msg], private val guess: Code) extends Msg {
 def getPlayer: ActorRef[Msg] = player
 def getGuess: Code = guess
}

object GuessMsg {
 def apply(player: ActorRef[Msg], guess: Code): GuessMsg = new GuessMsg(player, guess)
}

case class AllGuessesMsg(candidateWinner:ActorRef[Msg], private val guesses: Map[ActorRef[Msg], Code]) extends Msg {
 def getGuesses: Map[ActorRef[Msg],Code] = guesses
}

object AllGuessesMsg {
 def apply(winner:ActorRef[Msg],guesses: Map[ActorRef[Msg], Code]): AllGuessesMsg = new AllGuessesMsg(winner,guesses)
}

//
//  Referee Messages
//

// Only for the GUI so just names
case class TurnOrderMsg(private val turns: List[String]) extends Msg {
 def getTurns: List[String] = turns
}

object TurnOrderMsg {
 def apply(turns: List[String]): TurnOrderMsg = new TurnOrderMsg(turns)
}

case class YourTurnMsg(private val player: ActorRef[Msg]) extends Msg {
 def getPlayer : ActorRef[Msg] = player
}

object YourTurnMsg {
 def apply(player: ActorRef[Msg]): YourTurnMsg = new YourTurnMsg(player)
}

case class GuessResponseMsg(private val player: ActorRef[Msg], private val guess: Code, private val response: Response) extends Msg {
 def getPlayer: ActorRef[Msg] = player
 def getGuess: Code = guess
 def getResponse: Response = response
}

object GuessResponseMsg {
 def apply(player: ActorRef[Msg], guess: Code, response: Response): GuessResponseMsg = new GuessResponseMsg(player, guess, response)
}
case class TurnEnd(player:ActorRef[Msg]) extends Msg {
}
case class VictoryConfirmMsg(private val player: ActorRef[Msg]) extends Msg {
 def getPlayer: ActorRef[Msg] = player
}

object VictoryConfirmMsg {
 def apply(player: ActorRef[Msg]): VictoryConfirmMsg = new VictoryConfirmMsg(player)
}

case class VictoryDenyMsg(private val player: ActorRef[Msg]) extends Msg {
 def getPlayer: ActorRef[Msg] = player
}

object VictoryDenyMsg {
 def apply(player: ActorRef[Msg]): VictoryDenyMsg = new VictoryDenyMsg(player)
}

//
//  Game Messages
//

case class StartGameMsg(private val codeLength: Int, private val sendGuessToOthers: Boolean, private val players: List[ActorRef[Msg]], private val referee: ActorRef[Msg]) extends Msg {
 def getCodeLength = codeLength
 def getSendGuessToOthers = sendGuessToOthers
 def getPlayers = players
 def getReferee = referee
}

object StartGameMsg {
 def apply(codeLength: Int, sendGuessToOthers: Boolean, players: List[ActorRef[Msg]], referee: ActorRef[Msg]): StartGameMsg = new StartGameMsg(codeLength, sendGuessToOthers, players, referee)
}

case class StopGameMsg() extends Msg {
}

object StopGameMsg {
 def apply(): StopGameMsg = new StopGameMsg()
}

case class InitializeControllerMsg(private val nPlayers: Int, private val codeLength: Int, private val withHuman: Boolean, private val sharedResponses: Boolean) extends Msg {
 def getPlayers: Int = nPlayers
 def getLength: Int = codeLength
 def getHuman: Boolean = withHuman
 def getResponses: Boolean = sharedResponses
}

object InitializeControllerMsg {
 def apply(nPlayers: Int, codeLength: Int, withHuman: Boolean, sharedResponses: Boolean): InitializeControllerMsg = new InitializeControllerMsg(nPlayers, codeLength, withHuman, sharedResponses)
}


