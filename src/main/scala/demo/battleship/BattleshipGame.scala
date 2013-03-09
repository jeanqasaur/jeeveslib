package cap.jeeveslib.demo.battleship

import cap.jeeveslib.ast._
import cap.jeeveslib.jeeves._

/**
 * Battleship game implementation.
 * @author jean
 */
object BattleshipGame extends JeevesLib[User] {
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
}
