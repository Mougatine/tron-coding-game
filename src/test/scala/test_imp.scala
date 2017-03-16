import imperative.Player._
import org.scalatest._

class TestsImp extends FlatSpec with Matchers {
  "A grid" should "be initialized to Empty value" in {
    val grid = initGrid()
    grid.exists(tile => tile != Empty()) should be (false)
  }

  "A grid" should "have a tile occupied" in {
    val grid = initGrid()

    setValAt(grid, Occupied(1), 5, 5)
    getValAt(grid, 5, 5) should be (Occupied(1))
  }

  "A grid" should "have a tile predicted" in {
    val grid = initGrid()

    setValAt(grid, Predicted(1), 5, 5)
    getValAt(grid, 5, 5) should be (Predicted(1))
  }

  "A grid" should "takes in account dead player" in {
    val grid = initGrid()

    setValAt(grid, Occupied(0), 5, 5)
    setDead(grid, 0)
    getValAt(grid, 5, 5) should be (Empty())
  }

}

