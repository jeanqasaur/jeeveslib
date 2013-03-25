package cap.jeeveslib.demo.battleship

import cap.jeeveslib.ast._
import BattleshipGame._

case class GameContext(user: User, game: Game) extends Atom
