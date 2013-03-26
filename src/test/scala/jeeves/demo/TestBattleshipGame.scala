package test.cap.jeeveslib.jeeves.demo.battleship

import org.scalatest.FunSuite
import scala.collection.mutable.Map

import cap.jeeveslib.ast._
import cap.jeeveslib.demo.battleship._
import BattleshipGame._

class TestBattleshipGame extends FunSuite {
  val alice = User(0)
  val aliceBoard = Board(alice)
  val aliceBomb = Bomb(alice)

  val bob = User(1)
  val bobBoard = Board(bob)
  val bobBomb = Bomb(bob)

  val game = Game(Map(alice -> aliceBoard, bob -> bobBoard))
  val aliceCtxt = GameContext(alice, game)
  val bobCtxt =  GameContext(bob, game)

  test ("Can only put pieces on the board") {
    expectResult(true) {
      aliceBoard.placeShip(aliceCtxt, Carrier(alice), Point(0, 0), Point(0, 5))
    }
    // Cannot place the same piece again.
    expectResult(false) {
      aliceBoard.placeShip(aliceCtxt, Carrier(alice), Point(0,0), Point(0, 5))
    }

    // Cannot place another piece atthe same location.
    expectResult(false) {
      aliceBoard.placeShip(
        aliceCtxt, Battleship(alice), Point(0, 0), Point(0, 4))
    }

    expectResult(false) {
      aliceBoard.allPlaced()
    }
  }

  test ("Cannot place bombs until all pieces have been placed") {
    expectResult(false) {
      aliceBoard.getSquare(0, 0).bomb(bobCtxt, bobBomb)
    }
  }

  test ("Bob cannot put a ship on Alice's board") {
    expectResult(false) {
      aliceBoard.placeShip(bobCtxt, Battleship(bob), Point(1, 0), Point(1, 4))
    }
  }

  test ("Putting the rest of of Alice's pieces") {
    expectResult(true) {
      aliceBoard.placeShip(
        aliceCtxt, Battleship(alice), Point(1, 0), Point(1, 4))
    }
    expectResult(true) {
      aliceBoard.placeShip(aliceCtxt, Cruiser(alice), Point(2, 0), Point(2, 3))
    }
    expectResult(true) {
      aliceBoard.placeShip(
        aliceCtxt, Destroyer(alice), Point(3, 0), Point(3, 2))
    }
    expectResult(true) {
      aliceBoard.placeShip(
        aliceCtxt, Destroyer(alice), Point(4, 0), Point(4, 2))
    }
    expectResult(true) {
      aliceBoard.placeShip(
        aliceCtxt, Submarine(alice), Point(5, 0), Point(5, 1))
    }
    expectResult(true) {
      aliceBoard.placeShip(
        aliceCtxt, Submarine(alice), Point(5, 1), Point(5, 2))
    }
  }

  test ("Cannot put pieces after they have already been placed") {
    expectResult(false) {
      aliceBoard.placeShip(
        aliceCtxt, Submarine(alice), Point(6,0), Point(6, 1))
    }
  }

  test ("Cannot put bombs until all ships have been placed") {
    expectResult(false) {
      aliceBoard.getSquare(0, 0).bomb(bobCtxt, bobBomb)
    }
  }

  test ("Putting all of Bob's pieces") {
    expectResult(true) {
      bobBoard.placeShip(
        bobCtxt, Carrier(bob), Point(0, 0), Point(0, 5))
    }
    expectResult(true) {
      bobBoard.placeShip(
        bobCtxt, Battleship(bob), Point(1, 0), Point(1, 4))
    }
    expectResult(true) {
      bobBoard.placeShip(bobCtxt, Cruiser(bob), Point(2, 0), Point(2, 3))
    }
    expectResult(true) {
      bobBoard.placeShip(
        bobCtxt, Destroyer(bob), Point(3, 0), Point(3, 2))
    }
    expectResult(true) {
      bobBoard.placeShip(
        bobCtxt, Destroyer(bob), Point(4, 0), Point(4, 2))
    }
    expectResult(true) {
      bobBoard.placeShip(
        bobCtxt, Submarine(bob), Point(5, 0), Point(5, 1))
    }
    expectResult(true) {
      bobBoard.placeShip(
        bobCtxt, Submarine(bob), Point(5, 1), Point(5, 2))
    }
  }

  test ("Can bomb a piece with no ship") {
    expectResult(None) { game.bomb(aliceCtxt, bob, 9, 9) }
  }

  test ("Can bomb a piece with a ship") {
    expectResult(Some(Carrier(alice))) { game.bomb(bobCtxt, alice, 0, 0) }
  }

  test ("Cannot put two bombs in a row") {
    expectResult(None) { game.bomb(bobCtxt, alice, 0, 0) }
  }

  test ("Can see ship if bombed") {
    expectResult(Carrier(alice)) {
      concretize(bobCtxt, aliceBoard.getSquare(0, 0).getShip())
    }
    expectResult(Carrier(alice)) {
      concretize(bobCtxt, aliceBoard.getSquare(0, 3).getShip())
    }             
  }

  test ("Cannot see ship if not bombed") {
    expectResult(NoShip) {
      concretize(aliceCtxt, bobBoard.getSquare(0, 0).getShip())
    }
  }

  test ("Playing the rest of the game..." ) {
    expectResult(Some(Carrier(bob))) {
      game.bomb(aliceCtxt, bob, 0, 0)
    }
    expectResult(Some(Battleship(alice))) {
      game.bomb(bobCtxt, alice, 1, 0)
    }
    expectResult(Some(Battleship(bob))) {
      game.bomb(aliceCtxt, bob, 1, 0)
    }
    expectResult(Some(Cruiser(alice))) {
      game.bomb(bobCtxt, alice, 2, 0)
    }
    expectResult(Some(Cruiser(bob))) {
      game.bomb(aliceCtxt, bob, 2, 0)
    }
    expectResult(Some(Destroyer(alice))) {
      game.bomb(bobCtxt, alice, 3, 0)
    }
    expectResult(Some(Destroyer(bob))) {
      game.bomb(aliceCtxt, bob, 3, 0)
    }
    expectResult(Some(Destroyer(alice))) {
      game.bomb(bobCtxt, alice, 4, 0)
    }
    expectResult(Some(Destroyer(bob))) {
      game.bomb(aliceCtxt, bob, 4, 0)
    }
    expectResult(Some(Submarine(alice))) {
      game.bomb(bobCtxt, alice, 5, 0)
    }
    expectResult(Some(Submarine(bob))) {
      game.bomb(aliceCtxt, bob, 5, 0)
    }
    expectResult(Some(Submarine(alice))) {
      game.bomb(bobCtxt, alice, 5, 1)
    }
    expectResult(true) {
      game.gameOver()
    }
  }

  test ("Cannot place ships once somebody has won") {
    expectResult(None) {
      game.bomb(aliceCtxt, bob, 5, 1)
    }
  }

  test ("Can see all ships once done") {
    expectResult(Submarine(bob))) {
      concretize(aliceCtxt, bobBoard.getSquare(5, 1).getShip())
    }
  }
}
