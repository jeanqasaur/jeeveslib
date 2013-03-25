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
  /*
  private val board0 = new Board()
  private val board1 = new Board()

  // TODO: Make server socket so we can have two players playing at once.
  def main(args: Array[String]) = {
    println("Welcome to this Battleship game.")
  
    // TODO: Listen until we get two players.
    while (!board0.hasLost() || !board1.hasLost()) {
      println("Player 0 board:")
      board0.printBoard()

      println("Player 1 board:")
      board1.printBoard()

      val input = readLine("prompt> ")
    }
  }
  */
}
