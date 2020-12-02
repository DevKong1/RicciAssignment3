package MasterMind.Utility

import scala.collection.mutable
import scala.util.Random

// Mastermind Code representation

class Code(/*codePoint: Int, pegs: Array[Int], code: String*/) {
  val RandomNum: Random = new Random()
  val codeRadix: Int = 10
  val codeLength: Int = 4
  val pegs: Array[Int] = Array.emptyIntArray
  val codePoint: Int = 0

  def codeRange: Int = Math.pow(codeRadix, codeLength).toInt

  //def getCodeRadix: Int = codeRadix
  //def getCodeLength: Int = codeLength

  def getRange: Set[Code] = {
    var allCodes: Set[Code] = Set.empty
    for(i <- 0 to codeRange) {
      allCodes ++= mutable.Set(new Code())
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
    for(i <- 0 to pegs.length) {
      pegs(i) match {
        case 0 =>
        case _ => q = (q + pegs(i) * Math.pow(10,i)).toInt
      }
    }
    q
  }

  def toPegs(codePoint: Int): Array[Int] = {
    val output: Array[Int] = new Array[Int](codeLength)
    var cP: Int = codePoint
    for(i <- 0 to codeLength) {
      output(i) = codePoint % codeRadix
      cP /= codeRadix
    }
    output
  }

  def getResponse(other: Code): Response = {
    val a: Array[Int] = Array.copyOf(pegs, pegs.length)
    val b: Array[Int] = Array.copyOf(other.pegs, other.pegs.length)
    var black: Int = 0
    var white: Int = 0
    for(i <- 0 to a.length) {
      if(a(i).equals(b(i))) {
        black+=1
        a(i) = -1
        b(i) = -2
      }
    }
    for(i <- 0 to a.length) {
      for(j <- 0 to b.length) {
        if(a(i).equals(b(i))) {
          white+=1
          b(j) = -2
          null
        }
      }
    }
    Response(black, white)
  }

  override def hashCode(): Int = codePoint

  override def equals(obj: Any): Boolean = {
    if (this eq obj.asInstanceOf[Object]) return true
    if (obj == null) return false
    if (getClass ne obj.getClass) return false
    //val other: Code = obj.asInstanceOf[Code]
    this.codePoint == obj.asInstanceOf[Code].codePoint
  }

  override def toString: String = {
    val str: Array[Char] = new Array[Char](pegs.length)
    for(i <- 0 to pegs.length) { str(i)=Character.forDigit(pegs(i), 10)}
    new String(str)
  }
}

object Code {
  def apply(): Code = new Code()
}

sealed trait CodeBreaker {
  def receiveKey(response: Response): Unit
}

class CodeBreakerImpl extends CodeBreaker {

  var response: Response = null
  var lastGuess: Code = null

  var impossible: List[Code] = List.empty
  var possible: Set[Code] = Set.empty

  def guess: Code = {
    var minimumEliminated: Int = -1
    var bestGuess: Code = null
    var unused: List[Code] = possible.toList
    unused ++= impossible
    for(a <- unused) {
      val minMaxTable: Array[Array[Int]] = Array.ofDim(Code().codeLength+1, Code().codeLength+1).asInstanceOf[Array[Array[Int]]]
      for(b <- possible) {
        val abResp: Response = a.getResponse(b)
        minMaxTable(abResp.getBlack)(abResp.getWhite)+=1
      }
      var mostHits: Int = -1
      for(row <- minMaxTable) {
        for(i <- row) {
          mostHits = Integer.max(i, mostHits)
        }
      }
      val score: Int = possible.size - mostHits
      if(score > minimumEliminated) {
        minimumEliminated = score
        bestGuess = a
      }
    }
    lastGuess = bestGuess
    bestGuess
  }

  override def receiveKey(response: Response): Unit = {
    this.response = response
    val iterator: Iterator[Code]  = possible.iterator
    while(iterator.hasNext) {
      val i: Code = iterator.next()
      if(!lastGuess.getResponse(i).equals(response)) {
        impossible ++= List(i)
        //iterator.remove
      }
    }
  }
}

object CodeBreakerImplObj {
  def apply(): CodeBreakerImpl = new CodeBreakerImpl()
}

sealed trait CodeMaker {
  def setAnswer: Unit
  def setAnswer(code: Code): Unit
  def getAnswer(): Code
  def verify(guess: Code): Response
}

class CodeMakerImpl extends CodeMaker {

  var answer: Code = null

  override def setAnswer: Unit = answer = Code()

  override def setAnswer(code: Code): Unit = this.answer = code

  override def getAnswer(): Code = answer

  override def verify(guess: Code): Response = answer.getResponse(guess)
}

object CodeMakerImplObj {
  def apply(): CodeMakerImpl = new CodeMakerImpl()
}


