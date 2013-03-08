package cap.jeeveslib.demo.battleship

import cap.jeeveslib.ast._
import BattleshipGame._

case class Point(x: Int, y: Int) {
  def distance(other: Point) = math.abs(x - other.x) + math.abs(y - other.y)
  def inLine(other: Point) = (x == other.x) || (y == other.y)
}

sealed trait GamePiece {
  val size: Int

  private var _placed = false
  private var _bombed = false
  private var _squares: List[Square] = Nil

  // TODO: Associate permissions with this.
  def placePiece() = _placed = true
  def isPlaced(): Boolean = _placed
  def bombPiece() = _bombed = true
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

  def addSquare(s: Square) = {
    _squares = s::_squares
  }
  def getSquares(): List[Square] = _squares
}
case object Carrier extends GamePiece { val size = 5 }
case object Battleship extends GamePiece { val size = 4 }
case object Cruiser extends GamePiece { val size = 3 }
case object Destroyer extends GamePiece { val size = 2 }
case object Submarine extends GamePiece { val size = 1 }

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
        cur.placePiece()
        
        // Update the relevant board pieces.
        cur.getPiecePoints(start, end) match {
          case Some(pts) => {
            pts.foreach {
              pt => {
                // Update the board with the ship and the ship with the board.
                val Point(x, y) = pt
                _board(x)(y).updateShip(cur)
                cur.addSquare(_board(x)(y))
              }
            }
            // Say we placed the ship.
            return true
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
          print(" ")
        }
      }
      print("\n")
    }    
  }
}
