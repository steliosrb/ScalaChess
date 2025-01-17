import scala.collection.mutable.ArrayBuffer

case class State(pieces: Array[Piece],
                 player: Color,
                 roundNum: Int,
                 inCheck: Boolean) {

  def getNewPiecesFromValidMove(
      positionsFromTo: (Position, Position)): Array[Piece] = {
    val (positionFrom, positionsTo) = positionsFromTo
    pieces
      .filterNot { piece =>
        piece.position == positionsTo
      }
      .map { piece =>
        if (piece.position == positionFrom)
          Piece(piece.color, piece.pieceType, positionsTo)
        else
          piece
      }
  }

  private def printRowTop(): Unit = {
    print(" ")
    for (_ <- 0 to 7) {
      print(Constants.RANK_SEPARATOR)
      print(Constants.FILE_SEPARATOR)
    }
    println(Constants.RANK_SEPARATOR)
  }

  private def printRowBottom(): Unit = {
    printRowTop()
    print(" ")
    for (i <- 0 to 7) {
      print(" ")
      Helper.printAtCenter(Constants.FILE_SEPARATOR.length / 2,
                           ('a'.toInt + i).toChar.toString)
    }
  }

  private def printCell(x: Int, y: Int): Unit = {

    val cellName = pieces
      .find(p => p.position.x == x && p.position.y == y)
      .map(_.toString)
      .getOrElse(" ")

    Helper.printAtCenter(Constants.FILE_SEPARATOR.length / 2, cellName)

  }

  private def printRow(y: Int): Unit = {
    for (x <- 0 to 7) {
      print(Constants.RANK_SEPARATOR)
      printCell(x, y)
    }
    println(Constants.RANK_SEPARATOR)
  }

  def printState(): Unit = {

    println()
    println(s"Round: $roundNum")
    for (y <- 0 to 7) {
      printRowTop()
      print(8 - y)
      printRow(y)
    }
    printRowBottom()
    println()
    val inCheckText = if (inCheck) "in check" else ""
    println(s"Player to play: $player $inCheckText")

  }

}

object State {

  val pieceTypesInitialFileArray =
    Array(Rock, Knight, Bishop, Queen, King, Bishop, Knight, Rock)

  def newGame: State = {

    val pieces = ArrayBuffer[Piece]()
    for (i <- 0 to 7) {
      pieces += Piece(Black, Pawn, Position(i, 1))
      pieces += Piece(White, Pawn, Position(i, 6))
      pieces += Piece(Black, pieceTypesInitialFileArray(i), Position(i, 0))
      pieces += Piece(White, pieceTypesInitialFileArray(i), Position(i, 7))
    }
    State(pieces.toArray, White, 0, inCheck = false)
  }

  def isKingUnderAttack(pieces: Array[Piece], color: Color): Boolean = {
    val kingOpt = pieces.find { piece =>
      piece.pieceType == King && piece.color == color
    }

    kingOpt match {
      case Some(king) =>
        /** Check if any of the opponents pieces has a valid move on king's position*/
        val opponents = pieces
          .filter { _.color == color.opponent }
        opponents.map { piece =>
          piece
            .validateMove(king.position,
                          pieces,
                          isOpponentPieceTakingMove = true)
    }
        pieces
          .filter { _.color == color.opponent }
          .exists(
            piece =>
              piece
                .validateMove(king.position,
                              pieces,
                              isOpponentPieceTakingMove = true))

      case None => false
    }

  }

  def applyValidMove(currentState: State,
                     positionsFromTo: (Position, Position)): State = {

    val newPieces = currentState.getNewPiecesFromValidMove(positionsFromTo)

    State(newPieces,
          currentState.player.opponent,
          currentState.roundNum + 1,
          isKingUnderAttack(newPieces, currentState.player.opponent))
  }
}
