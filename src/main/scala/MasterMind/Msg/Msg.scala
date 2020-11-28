package Msg

sealed trait Msg {
}

object Msg {
 def apply() : Option[Msg] = ???
}

//
//  Player Messages
//

case object GuessMsg extends Msg {
}

case object AllGuessesMsg extends Msg {
}

//
//  Referee Messages
//


case object GuessResponseMsg extends Msg {
}



//
//  Game Messages
//