package MasterMind.Utility

// Response given by the referee to a guess

class Response(private val black: Int, private val white: Int) {
  def getBlack: Int = black
  def getWhite: Int = white
}

object Response {
  def apply(black: Int, white: Int): Response = new Response(black, white)
}