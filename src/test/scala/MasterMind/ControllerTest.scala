package MasterMind

import org.scalatest.funsuite.AnyFunSuite
import Model._
import MasterMind.Utility._
import akka.actor.typed.ActorSystem

class ControllerTest extends AnyFunSuite {

  test("Controller is setup correctly") {
    val GameSystem = ActorSystem(GameController(), "GameSystem")
    GameSystem ! InitializeControllerMsg(4,4,withHuman = false,sharedResponses = false)
  }
}