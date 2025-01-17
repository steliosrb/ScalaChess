import scala.collection.mutable.ArrayBuffer
case class Position(x: Int, y: Int) {

  def translatePosition: String = {
    ('a'.toInt + x).toChar.toString + (8 - y).toString
  }

  def inBounds(): Boolean = {
    (0 to 7 contains x) && (0 to 7 contains y)
  }
}

object Position {

  def translateToPositions(
      positionArray: Array[Int]): Option[(Position, Position)] = {

    if (positionArray.length == 4) {
      val positionFrom = Position(positionArray(0), positionArray(1))
      val positionTo = Position(positionArray(2), positionArray(3))
      Some(positionFrom, positionTo)
    } else
      None

  }

  def generatePositionsVertical(
      fromX: Int,
      toX: Int,
      y: Int,
      includeToX: Boolean = false): Array[Position] = {

    Helper.generateRange(fromX, toX, includeToX).map(x => Position(x, y))

  }

  def generatePositionsHorizontal(
      fromY: Int,
      toY: Int,
      x: Int,
      includeToY: Boolean = false): Array[Position] = {

    Helper.generateRange(fromY, toY, includeToY).map(y => Position(x, y))

  }

  def generatePositionsDiagonal(fromX: Int,
                                toX: Int,
                                fromY: Int,
                                toY: Int,
                                includeTo: Boolean = false,
                                limit: Int = 7): Array[Position] = {

    if (fromX < 0 || fromX > limit || toX < 0 || toX > limit || fromY < 0 || fromY > limit || toY < 0 || toY > limit || fromX == toX || fromY == toY)
      Array.empty
    else {
      val positions = ArrayBuffer[Position]()
      val directionX = if (fromX > toX) -1 else 1
      val directionY = if (fromY > toY) -1 else 1
      var y = fromY + directionY
      for {
        x <- Range(fromX + directionX, toX, directionX)
      } {
        positions += Position(x, y)
        y += directionY
      }

      if (includeTo)
        positions += Position(toX, toY)

      positions.toArray
    }
  }

}
