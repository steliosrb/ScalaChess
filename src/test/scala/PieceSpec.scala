import org.scalatest.flatspec.AnyFlatSpec

class PieceSpec extends AnyFlatSpec {

  "validatePieceMove King" should "generate validate on correct movement of king" in {
    val king = Piece(White, King, Position(3, 3))
    val correctPositions =
      Array(Position(4, 3),
            Position(3, 2),
            Position(3, 4),
            Position(4, 4),
            Position(2, 2))
    for (position <- correctPositions) {
      assert(
        king
          .validatePieceMove(position, false)
      )
    }
  }
  "validatePieceMove King" should "generate invalidate on wrong movement of king" in {
    val king = Piece(White, King, Position(3, 3))
    val correctPositions =
      Array(Position(1, 3),
            Position(5, 3),
            Position(3, 1),
            Position(3, 5),
            Position(5, 5),
            Position(1, 1))
    for (position <- correctPositions) {
      assert(
        !king
          .validatePieceMove(position, false)
      )
    }
  }

  "validatePieceMove Bishop" should "generate validate on correct movement of bishop" in {
    val bishop = Piece(White, Bishop, Position(3, 3))
    val correctPositions =
      Array(Position(4, 4), Position(1, 1), Position(5, 1), Position(1, 5))
    for (position <- correctPositions) {
      assert(
        bishop
          .validatePieceMove(position, false))
    }
  }
  "validatePieceMove Bishop" should "generate invalid on wrong movement of bishop" in {
    val bishop = Piece(White, Bishop, Position(3, 3))
    val correctPositions =
      Array(Position(1, 3), Position(3, 1), Position(5, 2), Position(2, 6))
    for (position <- correctPositions) {
      assert(
        !bishop
          .validatePieceMove(position, false))
    }
  }

  "validatePieceMove Rock" should "generate validate on correct movement of rock" in {
    val rock = Piece(White, Rock, Position(3, 3))
    val correctPositions =
      Array(Position(5, 3), Position(1, 3), Position(3, 5), Position(3, 1))
    for (position <- correctPositions) {
      assert(
        rock
          .validatePieceMove(position, false)
      )
    }
  }
  "validatePieceMove Rock" should "generate invalid on wrong movement of rock" in {
    val rock = Piece(White, Rock, Position(3, 3))
    val wrongPositions =
      Array(Position(5, 5), Position(1, 4), Position(4, 2), Position(0, 1))
    for (position <- wrongPositions) {
      assert(
        !rock
          .validatePieceMove(position, false)
      )
    }
  }

  "validatePieceMove Knight" should "generate validate correct movement of knight" in {
    val knight = Piece(White, Knight, Position(3, 3))
    val correctPositions =
      Array(Position(5, 4), Position(1, 2), Position(5, 2), Position(2, 5))
    for (position <- correctPositions) {
      assert(
        knight
          .validatePieceMove(position, false)
      )
    }
  }
  "validatePieceMove Knight" should "generate invalid on wrong movement of knight" in {
    val knight = Piece(White, Knight, Position(3, 3))
    val wrongPositions =
      Array(Position(1, 6), Position(0, 1), Position(5, 3), Position(1, 1))
    for (position <- wrongPositions) {
      assert(
        !knight
          .validatePieceMove(position, false)
      )
    }
  }

  "validatePieceMove Queen" should "generate validate on correct movement of queen" in {
    val queen = Piece(White, Queen, Position(3, 3))
    val correctPositions =
      Array(Position(4, 4),
            Position(1, 1),
            Position(5, 1),
            Position(1, 5),
            Position(5, 3),
            Position(1, 3),
            Position(3, 5),
            Position(3, 1))
    for (position <- correctPositions) {
      assert(
        queen
          .validatePieceMove(position, false))
    }
  }
  "validatePieceMove Queen" should "generate invalid on wrong movement of queen" in {
    val queen = Piece(White, Queen, Position(3, 3))
    val wrongPositions =
      Array(Position(5, 4), Position(1, 2), Position(5, 2), Position(2, 5))
    for (position <- wrongPositions) {
      assert(
        !queen
          .validatePieceMove(position, false))
    }
  }

  "validatePieceMove Pawn" should "generate validate on correct movement of pawn" in {
    val whitePawn = Piece(White, Pawn, Position(3, 3))
    val blackPawn = Piece(Black, Pawn, Position(3, 3))
    val whitePawnFirstMove = Piece(White, Pawn, Position(3, 6))
    val blackPawnFirstMove = Piece(Black, Pawn, Position(3, 1))
    assert(
      whitePawn
        .validatePieceMove(Position(3, 2), false)
    )
    assert(
      blackPawn
        .validatePieceMove(Position(3, 4), false)
    )
    assert(
      whitePawn
        .validatePieceMove(Position(4, 2), true)
    )
    assert(
      blackPawn
        .validatePieceMove(Position(2, 4), true)
    )
    assert(
      whitePawnFirstMove
        .validatePieceMove(Position(3, 4), false)
    )
    assert(
      blackPawnFirstMove
        .validatePieceMove(Position(3, 3), false)
    )
    assert(
      whitePawnFirstMove
        .validatePieceMove(Position(4, 5), true)
    )
    assert(
      blackPawnFirstMove
        .validatePieceMove(Position(2, 2), true)
    )
  }
  "validatePieceMove Pawn" should "generate invalid on wrong movement of pawn" in {
    val whitePawn = Piece(White, Pawn, Position(3, 3))
    val blackPawn = Piece(Black, Pawn, Position(3, 3))
    val whitePawnFirstMove = Piece(White, Pawn, Position(3, 6))
    val blackPawnFirstMove = Piece(Black, Pawn, Position(3, 1))
    assert(
      !whitePawn
        .validatePieceMove(Position(4, 2), false)
    )
    assert(
      !blackPawn
        .validatePieceMove(Position(2, 4), false)
    )
    assert(
      !whitePawn
        .validatePieceMove(Position(2, 4), true)
    )
    assert(
      !blackPawn
        .validatePieceMove(Position(4, 2), true)
    )
    assert(
      !whitePawnFirstMove
        .validatePieceMove(Position(3, 7), false)
    )
    assert(
      !blackPawnFirstMove
        .validatePieceMove(Position(3, 4), false)
    )
  }

  "validateNoObstacleOnMove" should "generate valid positions on correct position" in {
    val queen = Piece(White, Queen, Position(3, 3))
    val correctPositions =
      Array(Position(4, 4),
            Position(1, 1),
            Position(5, 1),
            Position(1, 5),
            Position(5, 3),
            Position(1, 3),
            Position(3, 5),
            Position(3, 1))
    for (position <- correctPositions) {
      assert(
        queen
          .validateNoObstacleOnMove(position, State.newGame.pieces))
    }

  }
  "validateNoObstacleOnMove" should "generate invalid positions on wrong position" in {
    val queen = Piece(White, Queen, Position(3, 3))
    val correctPositions =
      Array(Position(3, 7),
            Position(3, 0),
            Position(6, 0),
            Position(7, 7))
    for (position <- correctPositions) {
      assert(
        !queen
          .validateNoObstacleOnMove(position, State.newGame.pieces))
    }

  }

}
