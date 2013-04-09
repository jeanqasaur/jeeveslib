package cap.jeeveslib.demo.battleship

import cap.jeeveslib.ast._
import BattleshipGame._

case class Square(val owner: User) extends Atom {
  /* Policy definitions. */
  private def isOwner(ctxt: ObjectExpr[GameContext]) = { ctxt.user === owner }
  private def hasTurn(ctxt: ObjectExpr[GameContext]): Formula = {
    ctxt.applyFunction(ctxt => ctxt.game.hasTurn(ctxt.user))
  }
  private def allShipsPlaced(ctxt: ObjectExpr[GameContext]): Formula = {
    ctxt.applyFunction(ctxt => ctxt.game.allShipsPlaced())
  }
  private def gameOver(ctxt: ObjectExpr[GameContext]): Formula = {
    ctxt.applyFunction(ctxt => ctxt.game.gameOver())
  }
  private def mkShipSecret(ship: GamePiece): ObjectExpr[GamePiece] = {
    val a = mkLabel("ship");
    restrict(a
      , ctxt =>
          hasBomb() || isOwner(ctxt) || gameOver(ctxt));
    mkSensitive(a, ship, NoShip)
  }

  /* Ships. */
  private val _shipRef =
    new ProtectedObjectRef[GameContext, GameContext](NoShip
      // Policy for updating: must be owner and there can't be a ship there
      // already.
      , Some((ship: ObjectExpr[Atom], ic: ObjectExpr[GameContext]) =>
          isOwner(ic) && (ship === NoShip))
      , None
      , "hasShip")(BattleshipGame)

  // Policy: can put a ship where there wasn't a ship before.
  def updateShip(ctxt: GameContext, ship: GamePiece): Boolean = {
    _shipRef.update(ctxt, ctxt, mkShipSecret(ship)) == Success
  }
  def hasShip(): Formula = !(_shipRef.v === NoShip)
  def getShip(): ObjectExpr[GamePiece] = {
    _shipRef.v.asInstanceOf[ObjectExpr[GamePiece]]
  }

  /* Bombs. */
  private var _hasBombRef =
    new ProtectedObjectRef[GameContext, GameContext](NULL
      , Some((_: ObjectExpr[Atom], ic: ObjectExpr[GameContext]) =>
          allShipsPlaced(ic) && hasTurn(ic) && !gameOver(ic))
      , None
      , "hasBomb")(BattleshipGame)
  def bomb(ctxt: GameContext, bomb: Bomb): Boolean = {
    _hasBombRef.update(ctxt, ctxt, bomb) == Success
  }
  def hasBomb(): Formula = !(_hasBombRef.v === NULL)
}
