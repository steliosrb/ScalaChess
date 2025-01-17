sealed trait Color {
  def name: String
  def opponent: Color
}

case object Black extends Color {
  val name = "Black"
  def opponent: Color = White
}

case object White extends Color {
  val name = "White"
  def opponent: Color = Black
}
