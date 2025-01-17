object Validator {

  def validateMove(currentState: State,
                   positionsFromTo: (Position, Position)): Boolean = {
    val (positionFrom, positionTo) = positionsFromTo

    val pieceOptToMove = currentState.pieces.find { piece =>
      piece.position == positionFrom && piece.color == currentState.player
    }

    val pieceOptOnPositionTo = currentState.pieces.find { piece =>
      piece.position == positionTo
    }

    positionFrom.inBounds() &&
      positionTo.inBounds() &&
      pieceOptToMove.isDefined &&
      pieceOptOnPositionTo.forall(piece =>
        piece.color != pieceOptToMove.get.color) match {
      case true =>
        val pieceToMove = pieceOptToMove.get
        if (pieceToMove.validateMove(positionTo,
                                     currentState.pieces,
                                     pieceOptOnPositionTo.isDefined)) {

          /**
            * Check validation
            */
          !State.isKingUnderAttack(
            currentState.getNewPiecesFromValidMove(positionsFromTo),
            currentState.player)
        } else {
          false
        }
      case false => false
    }

  }

}
