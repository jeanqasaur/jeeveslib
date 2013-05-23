package cap.jeeveslib.demo.battleship

import cap.jeeveslib.ast.{Atom, BoolVal, Formula, ObjectExpr, ProtectedBoolRef, Success}
import BattleshipGame._

sealed trait GamePiece extends Atom {
  val size: Int
  val owner: User

  private def isOwner(ctxt: ObjectExpr[GameContext]) = { ctxt.user === owner }

  private val _placedRef =
    new ProtectedBoolRef[GameContext, GameContext](false
      , Some(hasShip => ic => !hasShip && isOwner(ic))
      , None
      , None, "isPlaced")(BattleshipGame)
  private var _placed = false

  private val _bombedRef =
    new ProtectedBoolRef[GameContext, GameContext](false
      , Some(hasBomb => ic => !hasBomb)
      , None
      , None, "isPlaced")(BattleshipGame)
  private var _bombed = false

  private var _squares: List[Square] = Nil

  def placePiece(ctxt: ObjectExpr[GameContext]): Boolean = {
    if (_placedRef.update(ctxt, ctxt, true) == Success) {
      _placed = true;
      true
    } else {
      false
    }
  }
  def isPlaced(): Boolean = _placed
  def bombPiece(ctxt: ObjectExpr[GameContext]): Boolean = {
    if (_bombedRef.update(ctxt, ctxt, true) == Success) {
      _bombed = true;
      true
    } else {
      false
    }
  }
  def isBombed(): Boolean = _bombed

  def getPiecePoints(start: Point, end: Point): Option[List[Point]] = {
    if (start.inLine(end) && start.distance(end) == size) {
      // If we are on the same horizontal line...
      if (start.x == end.x) {
        val yPts = if (start.y < end.y)
                    start.y until end.y else end.y until start.y
        Some(yPts.toList.map(yPt => Point(start.x, yPt)))
      } else {
        val xPts = if (start.x < end.x)
                    start.x until end.x else end.x until start.x
        Some(xPts.toList.map(xPt => Point(xPt, start.y)))
      }
    } else {
      return None
    }
  }

  // TODO: Return whether the update succeeded...
  def addSquare(s: Square): Boolean = {
    _squares = s::_squares
    return true
  }
  def getSquares(): List[Square] = _squares
}
case class Carrier(owner: User) extends GamePiece { val size = 5 }
case class Battleship(owner: User) extends GamePiece { val size = 4 }
case class Cruiser(owner: User) extends GamePiece { val size = 3 }
case class Destroyer(owner: User) extends GamePiece { val size = 2 }
case class Submarine(owner: User) extends GamePiece { val size = 1 }
case object NoShip extends GamePiece {
  val size = 0
  val owner = null
}
