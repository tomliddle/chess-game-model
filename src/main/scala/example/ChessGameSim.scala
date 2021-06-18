package example

sealed trait Colour
case object White extends Colour
case object Black extends Colour

sealed trait Name {
  def validMove(move: Move, color: Colour): Boolean
  def shortName: String
}
case object Pawn extends Name {
  override def validMove(move: Move, colour: Colour): Boolean = {
    (move.from.y + 1  == move.to.y) || (
        (colour == White && move.from.y == 1 && move.to.y == 3) ||
        (colour == Black && move.from.y == 6 && move.to.y == 4)
      )
  }

  override def shortName: String = "p"
}
case object Rook extends Name {
  override def validMove(move: Move, colour: Colour): Boolean = {
    move.xDiff == 0 || move.yDiff == 0
  }
  override def shortName: String = "r"
}
case object Knight extends Name {
  override def validMove(move: Move, colour: Colour): Boolean = {
    (move.xDiff == 1 && move.yDiff == 2) || (move.xDiff == 2 && move.yDiff == 1)
  }
  override def shortName: String = "n"
}
case object Bishop extends Name {
  override def validMove(move: Move, colour: Colour): Boolean = {
    move.xDiff % move.yDiff == 0
  }
  override def shortName: String = "b"

}
case object Queen extends Name {
  override def validMove(move: Move, colour: Colour): Boolean = {
    move.xDiff % move.yDiff == 0 || move.xDiff == 0 || move.yDiff == 0
  }
  override def shortName: String = "q"
}
case object King extends Name {
  override def validMove(move: Move, colour: Colour): Boolean = {
    move.xDiff <= 1 && move.yDiff <= 1
  }
  override def shortName: String = "k"
}

case class Piece(colour: Colour, name: Name) {
  val oppositeColour = if (colour == Black) White else colour
}
case class Location(x: Int, y: Int)
case class Move(from: Location, to: Location) {
  val xDiff: Int = scala.math.abs(from.x - to.x)
  val yDiff: Int = scala.math.abs(from.y - to.y)

  lazy val locations: Seq[Location] = Seq.range(from.x, to.x).zip(Seq.range(from.y, to.y)).map{ case (x, y) => Location(x, y) }

  override def toString: String = s"(${from.x},${from.y})-(${to.x}-${to.y})"
}

sealed trait Square {
  val location: Location

  def pieceAtLocation(colour: Colour): Boolean

  def toOption: Option[OccupiedSquare]
}
case class OccupiedSquare(piece: Piece, location: Location) extends Square {
  override def toString: String = if (piece.colour == Black) piece.name.shortName.toUpperCase else piece.name.shortName

  override def pieceAtLocation(colour: Colour): Boolean = piece.colour == colour

  def validMove(newLocation: Location): Boolean = piece.name.validMove(Move(location, newLocation), piece.colour)

  def nearbyEmptySquares: Seq[EmptySquare] = Seq.empty

  override def toOption: Option[OccupiedSquare] = Some(this)

}
case class EmptySquare(location: Location) extends Square {
  override def toString: String = "-"

  override def pieceAtLocation(colour: Colour): Boolean = false

  override def toOption: Option[OccupiedSquare] = None
}

case class Board(grid: Seq[Seq[Square]] = Seq.tabulate(8, 8){ case (x, y) => EmptySquare(Location(x, y))}) {

  private def findPieces(filterFn: OccupiedSquare => Boolean): Seq[OccupiedSquare] = {
    grid.flatMap { row =>
      row.flatMap { square => square.toOption.filter(filterFn) }
    }
  }

  def occupiedSquares(colour: Colour): Seq[OccupiedSquare] = findPieces(os => os.piece.colour == colour)
  def occupiedSquares(colour: Colour, name: Name): Seq[OccupiedSquare] = findPieces(os => os.piece.colour == colour && os.piece.name == name)

  def get(location: Location): Square = grid(location.x)(location.y)
  def getOccupied(location: Location): Option[OccupiedSquare] = get(location).toOption

  def removePiece(os: OccupiedSquare): Board = {
    val xRow: Seq[Square] = grid(os.location.x).updated(os.location.y, EmptySquare(os.location))
    Board(grid.updated(os.location.x, xRow))
  }
  def squareAt(location: Location): Square = grid(location.x)(location.y)
  def pieceAt(location: Location): Option[Piece] = squareAt(location) match {
    case o: OccupiedSquare => Some(o.piece)
    case _ => None
  }

  def addPiece(piece: Piece, location: Location): Board = {
    val xRow: Seq[Square] = grid(location.x).updated(location.y, OccupiedSquare(piece, location))
    Board(grid.updated(location.x, xRow))
  }

  def piecesInTheWay(os: OccupiedSquare, location: Location): Boolean = {
    os.piece.name match {
      case Knight => false
      case _ => Move(os.location, location).locations.drop(1).dropRight(1).exists(l => pieceAt(l).nonEmpty)
    }
  }

  def sameColourAtTargetLocation(os: OccupiedSquare, location: Location): Boolean = pieceAt(location).exists(p => p.colour == os.piece.colour)

  def isValidMove(os: OccupiedSquare, location: Location): Boolean = {
    os.validMove(location) && !piecesInTheWay(os, location) && !sameColourAtTargetLocation(os, location)
  }

  def movePiece(os: OccupiedSquare, location: Location): Board = {
    removePiece(os).addPiece(os.piece, location)
  }

  def isInCheck(colour: Colour): Boolean = {
    val king = occupiedSquares(colour, King).head
    listOfSquaresThatCanTake(king).nonEmpty
  }

  def listOfSquaresThatCanTake(target: OccupiedSquare): Seq[OccupiedSquare] = {
    occupiedSquares(target.piece.oppositeColour).flatMap { os =>
      if (isValidMove(os, target.location)) Some(os)
      else None
    }
  }

  def kingCannotMove(colour: Colour): Boolean = {
    val king = occupiedSquares(colour, King).head
    king.nearbyEmptySquares.isEmpty
  }

  def isInCheckMate(color: Colour): Boolean = {
    isInCheck(color) && kingCannotMove(color)
  }

  def printBoard(): Board = {
    (0 to 7).foreach { yVal =>
      val line = (0 to 7).foldLeft(Seq[String]()) { (strSeq, xVal) =>
        strSeq :+ grid(xVal)(yVal).toString
      }
      println(line.mkString(","))
    }
    println("")
    this
  }
}

class ChessGameSim {

  val moves = Seq[Move](
    Move(Location(0, 1), Location(0, 3)),
    Move(Location(1, 1), Location(1, 2)),
    Move(Location(0, 0), Location(0, 2))
  )

  val rook = Piece(White, Rook)
  val whitePawn = Piece(White, Pawn)
  val blackPawn = Piece(Black, Pawn)
  val blackKing = Piece(Black, King)
  val whiteKing = Piece(White, King)
  val blackQueen = Piece(Black, Queen)
  val whiteQueen = Piece(White, Queen)
  val whiteRook = Piece(White, Rook)
  val whiteKnight = Piece(White, Knight)
  val whiteBishop = Piece(White, Bishop)
  val blackRook = Piece(Black, Rook)
  val blackKnight = Piece(Black, Knight)
  val blackBishop = Piece(Black, Bishop)

  def addPieces: Board = {
    (0 to 7).foldLeft(Board()) { (currBoard, xPos) =>
      currBoard
        .addPiece(whitePawn, Location(xPos, 1))
        .addPiece(blackPawn, Location(xPos, 6))
    }
      .addPiece(whiteKing, Location(4, 0))
      .addPiece(whiteQueen, Location(3, 0))
      .addPiece(whiteRook, Location(0, 0))
      .addPiece(whiteRook, Location(7, 0))
      .addPiece(whiteKnight, Location(1, 0))
      .addPiece(whiteKnight, Location(6, 0))
      .addPiece(whiteBishop, Location(2, 0))
      .addPiece(whiteBishop, Location(5, 0))

      .addPiece(blackKing, Location(4, 7))
      .addPiece(blackQueen, Location(3, 7))
      .addPiece(blackRook, Location(0, 7))
      .addPiece(blackRook, Location(7, 7))
      .addPiece(blackKnight, Location(1, 7))
      .addPiece(blackKnight, Location(6, 7))
      .addPiece(blackBishop, Location(2, 7))
      .addPiece(blackBishop, Location(5, 7))
  }


  val board = addPieces

  board.printBoard()

  moves.foldLeft(board) { (currBoard, currMove) =>
    val piece = currBoard.get(currMove.from)


    piece.toOption.fold(currBoard) { os =>
      val validMove = currBoard.isValidMove(os, currMove.to)
      println(s"$currMove is valid $validMove")
      if (validMove) {
        currBoard.movePiece(os, currMove.to).printBoard()
      }
      else currBoard
    }
  }
}

object ChessGameSim extends App {
  new ChessGameSim()
}