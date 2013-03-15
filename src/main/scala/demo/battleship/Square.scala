package cap.jeeveslib.demo.battleship

import cap.jeeveslib.ast._
import BattleshipGame._

case class Square() extends Atom {
  private var _hasShip = false
  private var _hasBomb = false
  private var _ship: Option[GamePiece] = None

  // Policy: can put a ship where there wasn't a ship before.
  // TODO: Would it be useful to resolve the constraint right away so we can
  // do more declarative programming?
  def updateShip(ship: GamePiece): Boolean = {
    _ship = Some(ship)
    _hasShip = true
    return true
  }
  def hasShip(): Boolean = _hasShip
  def getShip(): Option[GamePiece] = _ship
  def bomb(): Unit = { _hasBomb = true }
  def hasBomb(): Boolean = _hasBomb
}
