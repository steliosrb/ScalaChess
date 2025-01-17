import org.scalatest.flatspec.AnyFlatSpec

class PositioningSpec extends AnyFlatSpec {

  "generatePositionsVertical" should "generate correct positions" in {

    val paramsResults = Array(
      (3, 6, 1, false, Array(Position(4, 1), Position(5, 1))),
      (6, 3, 1, false, Array(Position(5, 1), Position(4, 1))),
      (6, 6, 1, false, Array.empty),
      (3, 6, 1, true, Array(Position(4, 1), Position(5, 1), Position(6, 1))),
      (6, 3, 1, true, Array(Position(5, 1), Position(4, 1), Position(3, 1))),
      (6, 6, 1, true, Array.empty),
      (6, 8, 1, false, Array.empty),
      (-1, 2, 1, false, Array.empty)
    )

    for (paramResult <- paramsResults) {
      assert(
        Position
          .generatePositionsVertical(
            paramResult._1,
            paramResult._2,
            paramResult._3,
            paramResult._4) sameElements paramResult._5)
    }

  }

  "generatePositionsHorizontal" should "generate correct positions" in {

    val paramsResults = Array(
      (3, 6, 1, false, Array(Position(1, 4), Position(1, 5))),
      (6, 3, 1, false, Array(Position(1, 5), Position(1, 4))),
      (6, 6, 1, false, Array.empty),
      (3, 6, 1, true, Array(Position(1, 4), Position(1, 5), Position(1, 6))),
      (6, 3, 1, true, Array(Position(1, 5), Position(1, 4), Position(1, 3))),
      (6, 6, 1, true, Array.empty),
      (6, 8, 1, false, Array.empty),
      (-1, 2, 1, false, Array.empty)
    )

    for (paramResult <- paramsResults) {
      assert(
        Position
          .generatePositionsHorizontal(
            paramResult._1,
            paramResult._2,
            paramResult._3,
            paramResult._4) sameElements paramResult._5)
    }
  }

  "generatePositionsDiagonal" should "generate correct positions" in {

    val paramsResults = Array(
      (3, 7, 3, 7, false, Array(Position(4, 4), Position(5, 5), Position(6, 6))),
      (7,
       0,
       0,
       7,
       false,
       Array(Position(6, 1),
             Position(5, 2),
             Position(4, 3),
             Position(3, 4),
             Position(2, 5),
             Position(1, 6))),
      (3, 6, 5, 2, false, Array(Position(4, 4), Position(5, 3))),
      (3, 0, 5, 2, false, Array(Position(2, 4), Position(1, 3))),
      (3, 1, 5, 7, false, Array(Position(2, 6))),
      (3, 5, 5, 7, false, Array(Position(4, 6))),
      (3, 3, 5, 7, false, Array.empty),
      (3, 5, 5, 5, false, Array.empty),
      (3, 3, 5, 5, false, Array.empty),
      (3, -5, 5, 7, false, Array.empty),
      (-3, 5, 5, 7, false, Array.empty),
      (3, 5, -5, 7, false, Array.empty),
      (3, 6, 5, 2, true, Array(Position(4, 4), Position(5, 3), Position(6, 2))),
      (3, 0, 5, 2, true, Array(Position(2, 4), Position(1, 3), Position(0, 2))),
      (3, 1, 5, 7, true, Array(Position(2, 6), Position(1, 7))),
      (3, 5, 5, 7, true, Array(Position(4, 6), Position(5, 7))),
      (3, 3, 5, 7, true, Array.empty),
      (3, 5, 5, 5, true, Array.empty),
      (3, 3, 5, 5, true, Array.empty)
    )

    for (paramResult <- paramsResults) {
      assert(Position
        .generatePositionsDiagonal(paramResult._1,
          paramResult._2,
          paramResult._3,
          paramResult._4,
          paramResult._5) sameElements paramResult._6)
    }

  }

  "translateToPositions" should "return correct result on correct data" in {
    val paramsResults = Array(
      (Array(1, 2, 3, 4), Some(Position(1, 2), Position(3, 4))),
      (Array(1, 2, 3), None)
    )

    for (paramResult <- paramsResults) {
      assert(Position.translateToPositions(paramResult._1) == paramResult._2,
             Position(3, 4))
    }

  }

  "translatePosition" should "return correct translation of position" in {
    val paramsResults = Array(
      (Position(1, 2), "b6"),
      (Position(2, 3), "c5")
    )

    for (paramResult <- paramsResults) {
      assert(paramResult._1.translatePosition == paramResult._2)
    }
  }

  "inBounds" should "return correct if is out of bound" in {
    val paramsResults = Array(
      Position(-1, 2),
      Position(1, -2),
      Position(1, -2),
      Position(-1, -2)
    )

    for (paramResult <- paramsResults) {
      assert(!paramResult.inBounds())
    }
    assert(!Position(-1, 2).inBounds())
  }
}
