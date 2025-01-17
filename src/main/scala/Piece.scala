case class Piece(color: Color, pieceType: PieceType, position: Position) {

  override def toString: String = {

    color match {
      case Black => pieceType.name.toLowerCase()
      case White => pieceType.name.toUpperCase()
    }
  }

  private def validateForwardMove(fromY: Int,
                                  toY: Int,
                                  fromX: Int,
                                  toX: Int,
                                  limit: Int,
                                  color: Color) = {
    color match {
      case Black => (1 to limit contains (toY - fromY)) && fromX == toX
      case White => (1 to limit contains (fromY - toY)) && fromX == toX
    }
  }

  def validateNoObstacleOnMove(inBoundsPositionTo: Position,
                               pieces: Array[Piece]): Boolean = {

    /**
      * No need to check Knight since is jumping to the destination
      * No need to check King since is moving only one cell and is already verified that
      * the piece is not the same color or is empty
      */
    val inBetweenPositions: Array[Position] =
      pieceType match {
        case King | Knight =>
          Array.empty
        case Queen =>
          if (position.y == inBoundsPositionTo.y) {
            Position.generatePositionsVertical(position.x,
                                               inBoundsPositionTo.x,
                                               position.y)
          } else if (position.x == inBoundsPositionTo.x) {
            Position.generatePositionsHorizontal(position.y,
                                                 inBoundsPositionTo.y,
                                                 position.x)
          } else {
            Position.generatePositionsDiagonal(position.x,
                                               inBoundsPositionTo.x,
                                               position.y,
                                               inBoundsPositionTo.y)
          }
        case Rock =>
          if (position.y == inBoundsPositionTo.y) {
            Position.generatePositionsVertical(position.x,
                                               inBoundsPositionTo.x,
                                               position.y)
          } else {
            Position.generatePositionsHorizontal(position.y,
                                                 inBoundsPositionTo.y,
                                                 position.x)
          }
        case Bishop =>
          Position.generatePositionsDiagonal(position.x,
                                             inBoundsPositionTo.x,
                                             position.y,
                                             inBoundsPositionTo.y)

        case Pawn =>
          Position.generatePositionsHorizontal(position.y,
                                               inBoundsPositionTo.y,
                                               position.x)

      }

    pieces.forall(piece => !inBetweenPositions.contains(piece.position))

  }

  def validatePieceMove(inBoundsPositionTo: Position,
                        isOpponentPieceTakingMove: Boolean = false): Boolean = {

    lazy val rockMoveValidation =
      (position.x == inBoundsPositionTo.x) ||
        (position.y == inBoundsPositionTo.y)

    lazy val bishopMoveValidation =
      Math.abs(position.x - inBoundsPositionTo.x) ==
        Math.abs(position.y - inBoundsPositionTo.y)

    pieceType match {
      case King =>
        Math.abs(position.x - inBoundsPositionTo.x) <= 1 &&
          Math.abs(position.y - inBoundsPositionTo.y) <= 1
      case Queen =>
        rockMoveValidation || bishopMoveValidation
      case Rock =>
        rockMoveValidation
      case Bishop =>
        bishopMoveValidation
      case Knight =>
        (Math.abs(position.x - inBoundsPositionTo.x) == 2 &&
          Math.abs(position.y - inBoundsPositionTo.y) == 1) ||
          (Math.abs(position.x - inBoundsPositionTo.x) == 1 &&
            Math.abs(position.y - inBoundsPositionTo.y) == 2)
      case Pawn =>
        val isFirstMove = color == White && position.y == 6 || color == Black && position.y == 1
        (!isOpponentPieceTakingMove &&
        isFirstMove &&
        validateForwardMove(position.y,
                            inBoundsPositionTo.y,
                            position.x,
                            inBoundsPositionTo.x,
                            2,
                            color)) ||
        (!isOpponentPieceTakingMove &&
        validateForwardMove(position.y,
                            inBoundsPositionTo.y,
                            position.x,
                            inBoundsPositionTo.x,
                            1,
                            color)) ||
        (isOpponentPieceTakingMove &&
        ((color == White && position.y - inBoundsPositionTo.y == 1 &&
        Math.abs(position.x - inBoundsPositionTo.x) == 1) ||
        (color == Black && inBoundsPositionTo.y - position.y == 1 &&
        Math.abs(position.x - inBoundsPositionTo.x) == 1)))
    }

  }

  def validateMove(inBoundsPositionTo: Position,
                   pieces: Array[Piece],
                   isOpponentPieceTakingMove: Boolean = false): Boolean = {

    validatePieceMove(inBoundsPositionTo, isOpponentPieceTakingMove) &&
    validateNoObstacleOnMove(inBoundsPositionTo, pieces)

  }

}
