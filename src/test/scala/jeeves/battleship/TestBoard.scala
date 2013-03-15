package test.cap.jeeveslib.jeeves.demo.battleship

import org.scalatest.FunSuite

import cap.jeeveslib.demo.battleship._

class TestBoard extends FunSuite {
  val testBoard = Board()

  test ("putting game pieces") {
    expectResult(true) {
      testBoard.placeShip(Carrier, Point(0, 0), Point(0, 5))
    }
    // Cannot place the same piece again.
    expectResult(false) {
      testBoard.placeShip(Carrier, Point(0,0), Point(0, 5))
    }

    expectResult(false) { testBoard.allPlaced() }
  }

  test ("putting bombs") {

  }
}
