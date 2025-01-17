import com.whitehatgaming.UserInputFile

import scala.collection.mutable.ArrayBuffer

object Game extends App {

  def gameLoop(userInputFile: UserInputFile) = {

    var validMoves = ArrayBuffer[String]()
    var currentState = State.newGame

    currentState.printState()

    var nextMove: Array[Int] = userInputFile.nextMove()

    while (nextMove != null && !nextMove.isEmpty) {

      val positions = Position.translateToPositions(nextMove) match {
        case Some((positionX, positionY)) => (positionX, positionY)
        case None                         => throw new Exception(s"Not valid move on file")
      }

      val translatedPositionStr =
        s"${positions._1.translatePosition}${positions._2.translatePosition}"

      if (Validator.validateMove(currentState, positions)) {
        println(s"Valid Move -> $translatedPositionStr")
        validMoves += translatedPositionStr
        currentState = State.applyValidMove(currentState, positions)
        currentState.printState()

      } else {
        println(s"Invalid Move -> $translatedPositionStr")
      }
      nextMove = userInputFile.nextMove()

    }
    validMoves.toArray

  }

  override def main(args: Array[String]) {

    val fileName =
      if (args.length > 0) args.head else "data/sample-moves-invalid.txt"

    println(s"Starting Game with input file $fileName ...")

    try {
      gameLoop(new UserInputFile(fileName))
    } catch {
      case e: Exception => print(e.getMessage)
    }

  }

}
