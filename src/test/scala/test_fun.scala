import functional.Player._
import org.scalatest._

class TestsFun extends FlatSpec with Matchers {
  "A grid" should "be initialized to Empty value" in {
    val grid = initGrid(Empty())
    grid.exists(tile => tile != Empty()) should be (false)
  }

  "A grid" should "have a tile occupied" in {
    val grid = initGrid(Empty())

    setValAt(grid, Occupied(1), 5, 5)
    getValAt(grid, 5, 5) should be (Occupied(1))
  }

}

