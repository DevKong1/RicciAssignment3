package MasterMind.Utility

// Response given by the referee to a guess
class Response(private val black: Int, private val white: Int) {
  def getBlack: Int = black
  def getWhite: Int = white

  override def hashCode(): Int = {
    var hash: Int = 7
    hash = 13*hash + this.black
    hash = 13*hash + this.white
    hash
  }

  override def equals(obj: Any): Boolean = {
    if (this eq obj.asInstanceOf[Object]) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    val other: Response = obj.asInstanceOf[Response]
    if(this.black != other.black) return false
    if(this.white != other.white) return false
    true
  }

  override def toString: String = "black=" + black + ", white=" + white
}

object Response {
  def apply(black: Int, white: Int): Response = new Response(black, white)
}