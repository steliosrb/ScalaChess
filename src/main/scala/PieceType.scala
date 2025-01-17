sealed trait PieceType {
  def fullName: String
  def name: String
}

case object King extends PieceType {
  val name = "K"
  val fullName = "King"
}

case object Queen extends PieceType {
  val name = "Q"
  val fullName = "Queen"
}

case object Rock extends PieceType {
  val name = "R"
  val fullName = "Rock"
}

case object Bishop extends PieceType {
  val name = "B"
  val fullName = "Bishop"
}

case object Knight extends PieceType {
  val name = "N"
  val fullName = "kNight"
}

case object Pawn extends PieceType {
  val name = "P"
  val fullName = "Pawn"
}
