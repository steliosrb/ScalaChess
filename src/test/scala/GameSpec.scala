import com.whitehatgaming.UserInputFile
import org.scalatest.flatspec.AnyFlatSpec

class GameSpec extends AnyFlatSpec {

  "gameLoop" should "return correct valid moves when all moves are valid" in {
    assert(Game
      .gameLoop(new UserInputFile("data/sample-moves.txt")) sameElements Array(
      "e2e4",
      "e7e5",
      "b1c3",
      "d7d6",
      "h2h3",
      "c8e6",
      "h1h2"))
  }

  "gameLoop" should "return correct valid moves when all moves are valid (checkmate)" in {
    assert(
      Game
        .gameLoop(new UserInputFile("data/sample-checkmate.txt")) sameElements Array(
        "e2e4",
        "e7e5",
        "f1c4",
        "b8c6",
        "d1f3",
        "d7d6",
        "f3f7"))
  }

  "gameLoop" should "return correct valid moves when not all moves are valid" in {
    assert(Game
      .gameLoop(new UserInputFile("data/sample-moves-invalid.txt")) sameElements Array(
      "e2e4",
      "e7e5",
      "h2h3"))
  }

  "gameLoop" should "return correct valid moves when not all moves are valid (check)" in {
    assert(
      Game
        .gameLoop(new UserInputFile("data/sample-moves-invalid-check.txt")) sameElements Array(
        "e2e4",
        "e7e5",
        "f1c4",
        "b8c6",
        "d1f3",
        "d7d5",
        "f3f6",
        "e8d7",
        "f6f7",
        "d7d6"))
  }

  "gameLoop" should "return correct valid moves when not all moves are valid (check) suicide" in {
    assert(
      Game
        .gameLoop(new UserInputFile("data/sample-moves-invalid-check-2.txt")) sameElements Array(
        "e2e4",
        "e7e6",
        "f1c4",
        "b8c6",
        "d1f3",
        "d7d5",
        "f3f6",
        "e8d7",
        "a2a3",
        "d7d6",
        "a3a4"))
  }

}
