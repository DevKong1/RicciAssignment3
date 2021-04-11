package MasterMind.Utility

import akka.actor.typed.DispatcherSelector

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Random

// Mastermind Code representation

class Code(length: Int) {
  private val codeRadix: Int = 10
  private val codeLength: Int = length
  private var pegs: Array[Int] = Array.emptyIntArray
  private var codePoint: Int = 0

  def codeRange: Int = Math.pow(codeRadix, codeLength).toInt

  //def getCodeRadix: Int = codeRadix
  def getLength: Int = codeLength

  def getRange: Set[Code] = {
    var allCodes: Set[Code] = Set()
    for(i <- 0 until codeRange) {
      allCodes += Code(getLength, i)
    }
    allCodes
  }

  def isValid(input: String): Boolean = input.length match {
    case this.codeLength =>
      val ch: Array[Char] = input.toCharArray
      for(i <- ch) {
        if(Character.isDigit(i)) {
          if(Character.digit(i, 10) >= codeRadix) { false }
          else { false }
        }
      }
      true
    case _ => false
  }

  def toCodePoint(pegs: Array[Int]): Int = {
    var q: Int = 0
    for(i <- pegs.indices) {
      pegs(i) match {
        case 0 =>
        case _ => q = q+(pegs(i) * Math.pow(10,i)).toInt
      }
    }
    q
  }

  def toPegs(codePoint: Int): Array[Int] = {
    val output: Array[Int] = new Array[Int](codeLength)
    var cP: Int = codePoint
    for(i <- 0 until codeLength) {
      output(i) = cP % codeRadix
      cP /= codeRadix
    }
    output
  }

  def getResponse(other: Code): Response = {
    val a: Array[Int] = Array.copyOf(pegs, pegs.length)
    val b: Array[Int] = Array.copyOf(other.pegs, other.pegs.length)
    var black: Int = 0
    var white: Int = 0
    for(i <- a.indices) {
      if(a(i).equals(b(i))) {
        black+=1
        a(i) = -1
        b(i) = -2
      }
    }
    for(i <- a.indices) {
      for(j <- b.indices) {
        if(a(i).equals(b(j))) {
          white+=1
          b(j) = -2
          null
        }
      }
    }
    Response(black, white)
  }

  override def hashCode(): Int = this.codePoint

  override def equals(obj: Any): Boolean = {
    if (this eq obj.asInstanceOf[Object]) return true
    if (obj == null) return false
    if (getClass != obj.getClass) return false
    this.codePoint == obj.asInstanceOf[Code].codePoint
  }

  override def toString: String = {
    val str = new Array[Char](pegs.length)
    for(i <- pegs.indices) { str(i)=Character.forDigit(pegs(i), 10)}
    new String(str)
  }
}

object Code {

  def apply(length: Int): Code = {
    val code: Code = new Code(length)
    code.codePoint = Random.nextInt(code.codeRange)
    code.pegs = code.toPegs(code.codePoint)
    code
  }

  def apply(length: Int, codePoint: Int): Code = {
    val code: Code = new Code(length)
    code.codePoint = codePoint
    code.pegs = code.toPegs(codePoint)
    code
  }

  def apply(length: Int, pegs: Array[Int]): Code = {
    val code: Code = new Code(length)
    code.pegs = pegs
    code.codePoint = code.toCodePoint(pegs)
    code
  }

  def apply(length: Int, num: String): Code = {
    val code: Code = new Code(length)
    val p: Array[Char] = num.toCharArray
    code.pegs = new Array[Int](p.length)
    for(i <- p.indices) {
      code.pegs(i) = Character.digit(p(i), 10)
    }
    code.codePoint = code.toCodePoint(code.pegs)
    code
  }
}

sealed trait CodeBreaker {
  def receiveKey(response: Response): Unit
}

class CodeBreakerImpl(length: Int, codeRange: Set[Code]) extends CodeBreaker {
  val codeLength: Int = length
  var response: Response = _
  var lastGuess: Option[Code] = Option.empty
  var isGuessing: Boolean = false

  var impossible: List[Code] = List()
  var possible: Set[Code] = codeRange
  def getGuess: Option[Code] = lastGuess
  def guess(implicit ec: ExecutionContext): Future[Unit] = Future{
      var aaa = 0
      isGuessing = true
      var minimumEliminated: Int = -1
      var bestGuess: Code = null
      var unused: List[Code] = possible.toList
      unused ++= impossible
      for (a <- unused) {
        val minMaxTable = Array.ofDim[Int](codeLength + 1, codeLength + 1)
        for (b <- possible) {
          val abResp: Response = a.getResponse(b)
          minMaxTable(abResp.getBlack)(abResp.getWhite) += 1
        }
        var mostHits: Int = -1
        for (row <- minMaxTable) {
          for (i <- row) {
            mostHits = Integer.max(i, mostHits)
          }
        }
        val score: Int = possible.size - mostHits
        if (score > minimumEliminated) {
          minimumEliminated = score
          bestGuess = a
        }
      }
      lastGuess = Option(bestGuess)
  }

  //TODO: Need to Test this function
  override def receiveKey(response: Response): Unit = {
    this.response = response
    val iterator: Iterator[Code] = possible.iterator
    while(iterator.hasNext) {
      val i: Code = iterator.next()
      if(lastGuess.isDefined && !lastGuess.get.getResponse(i).equals(response)) {
        impossible ++= List(i)
        //iterator.remove
        possible -= i
      }
    }
  }
}

object CodeBreakerImplObj {
  def apply(length: Int, codeRange: Set[Code]): CodeBreakerImpl = new CodeBreakerImpl(length, codeRange)
}