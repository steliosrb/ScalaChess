import scala.collection.mutable.ArrayBuffer
object Helper {

  def printAtCenter(offset: Int, str: String): Unit = {
    for (_ <- 0 until offset)
      print(" ")

    print(str)

    for (_ <- offset + str.length until Constants.FILE_SEPARATOR.length)
      print(" ")
  }

  def generateRange(from: Int,
                    to: Int,
                    includeTo: Boolean,
                    limit: Int = 7): Array[Int] = {
    if (from < 0 || from > limit || to < 0 || to > limit || from == to)
      Array.empty
    else {
      val rangeArray = ArrayBuffer[Int]()
      val direction = if (from > to) -1 else 1
      for (i <- Range(from + direction, to, direction))
        rangeArray += i
      if (includeTo)
        rangeArray += to
      rangeArray.toArray
    }
  }

}
