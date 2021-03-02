package MasterMind

import org.scalatest.funsuite.AnyFunSuite
import Model._

class ControllerTest extends AnyFunSuite {
  test("Controller is setup correctly") {
    val gameController = GameController()
    println(gameController)
    assert(gameController != null)
  }
}