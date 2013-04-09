package cap.jeeveslib.demo.battleship

import scala.collection.mutable.Map

import cap.jeeveslib.ast._
import cap.jeeveslib.jeeves._

/**
 * Battleship game implementation.
 * @author jean
 */
case class Game(boards: Map[User, Board]) extends Atom {
  case class NoSuchUserException(u: User) extends Throwable
  def getBoard(user: User): Board = {
    boards.get(user) match {
      case Some(b) => b
      case None => throw NoSuchUserException(user)
    }
  }
  
  def allShipsPlaced(): Boolean = {
    boards.forall { case (_, board) => board.allPlaced() }
  }

  def gameOver(): Boolean = {
    boards.exists { case (_, board) => board.hasLost() }
  }

  private var _moves: List[Bomb] = Nil
  def hasTurn(user: User) = {
    (_moves.isEmpty) || !(_moves.head.owner == user)
  }
  def bomb(ctxt: GameContext, user: User, x: Int, y: Int): Option[GamePiece] = {
    val (succeeded, piece) = getBoard(user).placeBomb(ctxt, x, y)
    if (BattleshipGame.concretize(ctxt, succeeded)) {
      _moves = Bomb(ctxt.user) :: _moves
      BattleshipGame.concretize(ctxt, piece).asInstanceOf[GamePiece] match {
        case NoShip => None
        case ship => Some(ship)
      }
    } else {
      None
    }
  }
}

object BattleshipGame extends JeevesLib[GameContext] {
  val alice = User(0)
  val aliceBoard = Board(alice)
  val aliceBomb = Bomb(alice)

  val bob = User(1)
  val bobBoard = Board(bob)
  val bobBomb = Bomb(bob)

  val game = Game(Map(alice -> aliceBoard, bob -> bobBoard))
  val aliceCtxt = GameContext(alice, game)
  val bobCtxt =  GameContext(bob, game)

  // TODO: Make server socket so we can have two players playing at once.
  def main(args: Array[String]): Unit = {
    println("Welcome to this Battleship game.")

    // Alice's pieces.
    aliceBoard.placeShip(aliceCtxt, Carrier(alice), Point(0, 0), Point(0, 5))
    aliceBoard.placeShip(
      aliceCtxt, Battleship(alice), Point(1, 0), Point(1, 4))
    aliceBoard.placeShip(aliceCtxt, Cruiser(alice), Point(2, 0), Point(2, 3))
    aliceBoard.placeShip(
      aliceCtxt, Destroyer(alice), Point(3, 0), Point(3, 2))
    aliceBoard.placeShip(
      aliceCtxt, Destroyer(alice), Point(4, 0), Point(4, 2))
    aliceBoard.placeShip(
      aliceCtxt, Submarine(alice), Point(5, 0), Point(5, 1))
    aliceBoard.placeShip(
      aliceCtxt, Submarine(alice), Point(5, 1), Point(5, 2))
  
    // Bob's pieces.
    bobBoard.placeShip(
        bobCtxt, Carrier(bob), Point(0, 0), Point(0, 5))
    bobBoard.placeShip(
      bobCtxt, Battleship(bob), Point(1, 0), Point(1, 4))
    bobBoard.placeShip(bobCtxt, Cruiser(bob), Point(2, 0), Point(2, 3))
    bobBoard.placeShip(
      bobCtxt, Destroyer(bob), Point(3, 0), Point(3, 2))
    bobBoard.placeShip(
      bobCtxt, Destroyer(bob), Point(4, 0), Point(4, 2))
    bobBoard.placeShip(
      bobCtxt, Submarine(bob), Point(5, 0), Point(5, 1))
    bobBoard.placeShip(
      bobCtxt, Submarine(bob), Point(5, 1), Point(5, 2))

    game.bomb(bobCtxt, alice, 0, 0)
    game.bomb(aliceCtxt, bob, 0, 0)
    game.bomb(bobCtxt, alice, 1, 0)
    game.bomb(aliceCtxt, bob, 1, 0)
    game.bomb(bobCtxt, alice, 2, 0)
    game.bomb(aliceCtxt, bob, 2, 0)
    game.bomb(bobCtxt, alice, 3, 0)
    game.bomb(aliceCtxt, bob, 3, 0)
    game.bomb(bobCtxt, alice, 4, 0)
    game.bomb(aliceCtxt, bob, 4, 0)
    game.bomb(bobCtxt, alice, 5, 0)
    game.bomb(aliceCtxt, bob, 5, 0)
    game.bomb(bobCtxt, alice, 5, 1)
    //game.gameOver()

    /*
    // TODO: Listen until we get two players.
    while (!board0.hasLost() || !board1.hasLost()) {
      println("Player 0 board:")
      board0.printBoard()

      println("Player 1 board:")
      board1.printBoard()

      val input = readLine("prompt> ")
    }
    */
  }
}
