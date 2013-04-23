package cap.jeeveslib.demo.battleship

import cap.jeeveslib.ast._
import BattleshipGame._

case class Point(x: Int, y: Int) {
  def distance(other: Point) = math.abs(x - other.x) + math.abs(y - other.y)
  def inLine(other: Point) = (x == other.x) || (y == other.y)
}

case class Board(val owner: User) extends Atom {
  case object OutOfBoundsException extends Throwable

  val boardSize = 10

  // Initialize board.
  private val _board: Array[Array[Square]] = new Array(boardSize)
  for (i <- 0 until boardSize) {
    val elem = _board(i)
    _board.update(i, new Array(boardSize))
    for (j <- 0 until boardSize) {
      _board(i).update(j, new Square(owner))
    }
  }

  private val _pieces: List[GamePiece] =
    List( Carrier(owner), Battleship(owner), Cruiser(owner)
        , Destroyer(owner), Destroyer(owner)
        , Submarine(owner), Submarine(owner))

  def getSquare(x: Int, y: Int): Square = { _board(x)(y) }

  // Question: How do we know the identities of each destroyer?
  def placeShip(ctxt: GameContext
    , ship: GamePiece, start: Point, end: Point): Boolean = {
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
                if (!_board(x)(y).updateShip(ctxt, cur)
                      || !cur.addSquare(_board(x)(y))) { return false; }
              }
            }
            return cur.placePiece(ctxt)
          }
          // If the points didn't fit, then we can't place the ship.
          case None => {
            println("Piece didn't fit: " + ship)
            return false
          }
        }
      }
    }
    println("Don't have piece to play: " + ship)
    return false
  }

  def placeBomb(ctxt: GameContext, x: Int, y: Int): ObjectExpr[GamePiece] = {
    if (x < boardSize && y < boardSize) {
      val boardShip = _board(x)(y).getShip();
      val bomb = Bomb(ctxt.user);
      val bombedPoint = _board(x)(y).bomb(ctxt, bomb)
      val succeeded: Formula = jif (
        (boardShip === NoShip)
        , (_: Unit) => { bombedPoint }
        , (_: Unit) => {
            boardShip.applyFunction((s: GamePiece) =>
              s.getSquares().forall {
                (square: Square) => square.bomb(ctxt, bomb) } &&
                  s.bombPiece(ctxt))});
     jif (succeeded, (_: Unit) => boardShip, (_: Unit) => NoShip)
    } else {
      println("Bomb location outside of board: (" + x + ", " + y + ")");
      throw OutOfBoundsException
    }
  }


  def allPlaced(): Boolean = {
    _pieces.forall(p => p.isPlaced())
  }
  def hasLost(): Boolean = {
    _pieces.forall(p => p.isBombed())
  }

  def printBoard(ctxt: ObjectExpr[GameContext]) = {
    for (j <- 0 until 10) {
      for (i <- 0 until 10) {
        val curSquare = _board(i)(j)
        if (concretize(ctxt, curSquare.hasBomb())) {
          print("X")
        } else if (concretize(ctxt, curSquare.hasShip())) {
          print("S")
        } else {
          print("W")
        }
      }
      print("\n")
    }    
  }

  def printRemainingPieces() = {
    println("Remaining pieces:")
    _pieces.foreach { p => if (!p.isBombed()) { println(p) } }
  }
}
