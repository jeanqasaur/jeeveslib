package cap.jeeveslib.demo.battleship

import cap.jeeveslib.ast._
import BattleshipGame._

case class Point(x: Int, y: Int) {
  def distance(other: Point) = math.abs(x - other.x) + math.abs(y - other.y)
  def inLine(other: Point) = (x == other.x) || (y == other.y)
}

case class Board() {
  // Initialize board.
  private val _board: Array[Array[Square]] = new Array(10)
  for (i <- 0 until 10) {
    val elem = _board(i)
    _board.update(i, new Array(10))
    for (j <- 0 until 10) {
      _board(i).update(j, new Square())
    }
  }

  private val _pieces: List[GamePiece] =
    List( Carrier, Battleship, Cruiser
        , Destroyer, Destroyer, Submarine, Submarine)

  // Question: How do we know the identities of each destroyer?
  def placeShip(ship: GamePiece, start: Point, end: Point): Boolean = {
    val it = _pieces.iterator
    while (it.hasNext) {
      val cur = it.next
      if (cur == ship && !cur.isPlaced()) {
        // Update the relevant board pieces.
        cur.getPiecePoints(start, end) match {
          case Some(pts) => {
            pts.foreach {
              pt => {
                // Update the board with the ship and the ship with the board.
                val Point(x, y) = pt
                if (!(_board(x)(y).updateShip(cur))) { return false; }
                if (!cur.addSquare(_board(x)(y))) { return false; }
              }
            }
            return cur.placePiece()
          }
          // If the points didn't fit, then we can't place the ship.
          case None => return false
        }
      }
    }
    return false
  }

  def placeBomb(x: Int, y: Int): Option[GamePiece] = {
    _board(x)(y).getShip() match {
      case Some(ship) => {
        ship.getSquares() foreach { square => square.bomb() }
        Some(ship)
      }
      case None =>
        _board(x)(y).bomb()
        None
    }
  }

  def allPlaced() = { _pieces.forall(p => p.isPlaced()) }
  def hasLost(): Boolean = { _pieces.forall(p => p.isBombed()) }

  def printBoard() = {
    for (j <- 0 until 10) {
      for (i <- 0 until 10) {
        val curSquare = _board(i)(j)
        if (curSquare.hasBomb()) {
          print("X")
        } else if (curSquare.hasShip()) {
          print("S")
        } else {
          print("W")
        }
      }
      print("\n")
    }    
  }
}
