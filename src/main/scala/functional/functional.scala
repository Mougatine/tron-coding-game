package functional

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player extends App {
  def width  = 30
  def height = 20

  sealed trait State
  case class Empty() extends State
  case class Occupied(id: Int) extends State

  def getValAt(grid: Array[State], x: Int, y: Int): State = {
    grid(x + y * width)
  }

  def setValAt(grid: Array[State], va: State, x: Int, y: Int): Unit =
    if (x != -1 || y != -1)
      grid(x + y * width) = va


  def initGrid(default: State): Array[State] = {
    def fillLine: Array[State] =
      Array.fill(width)(default)

    def fillGrid(y: Int, grid: Array[State]): Array[State] = {
      if (y >= height)
        grid
      else
        fillGrid(y + 1, grid ++ fillLine)
    }

    fillGrid(0, Array[State]())
  }

  def getValidNeighbours(grid: Array[State], x: Int, y: Int): Array[(Int, Int)] = {
    def possibleMoves: Array[(Int, Int)] =
      Array((0, -1), (-1, 0), (0, 1), (1, 0))

    def inGrid(x: Int, y: Int) =
      x >= 0 && x < width && y >= 0 && y < height

    def cond(x: Int, y: Int): Boolean =
      inGrid(x, y)  && getValAt(grid, x, y) == Empty()

    possibleMoves
      .map(fact => (x + fact._1, y + fact._2))
      .filter(coord => cond(coord._1, coord._2))
  }


  def computeScore(grid: Array[State], coord: (Int, Int), me: Int): String = {
    val movesAround: Array[(Int, Int)] =
      Array((0, -1), (-1, 0), (0, 1), (1, 0), (0, -2), (-1, -1),
        (-2, 0), (-1, 1), (0, 2), (1, 1), (2, 0), (1, -1))

    def validTile(grid: Array[State], x: Int, y: Int): Boolean =
      x >= 0 && x < width && y >= 0 && y < height && getValAt(grid, x, y) == Empty()

    def getScore(grid: Array[State], x: Int, y: Int): Int = {
      movesAround.map(move => (x + move._1, y + move._2))
        .count(move => validTile(grid, move._1, move._2))
    }

    def allMoves(grid: Array[State], x: Int, y: Int, depth: Int): Array[Int] =
      Array(helper(grid.clone, x, y - 1, depth),
        helper(grid.clone, x - 1, y, depth),
        helper(grid.clone, x, y + 1, depth),
        helper(grid.clone, x + 1, y, depth))

    def helper(grid: Array[State], x: Int, y: Int, depth: Int): Int = {
      if (!validTile(grid, x, y) || getValidNeighbours(grid, x, y).length <= 1)
        -10000000
      else if (depth >= 3)
        getScore(grid, x, y)
      else {
        setValAt(grid, Occupied(me), x, y)
        allMoves(grid, x, y, depth + 1).max
      }
    }

    val x = coord._1
    val y = coord._2
    val moves = Array((helper(grid.clone, x, y - 1, 0), "UP", (x , y - 1)),
      (helper(grid.clone, x - 1, y, 0), "LEFT", (x - 1, y)),
      (helper(grid.clone, x, y + 1, 0), "DOWN", (x , y + 1)),
      (helper(grid.clone, x + 1, y, 0), "RIGHT", (x + 1, y)))

    val correctMoves = moves .filter(move => validTile(grid, move._3._1, move._3._2))
    if (correctMoves.isEmpty)
      "UP" // We are dead anyway
    else
      correctMoves.maxBy(_._1)._2
  }


  val grid: Array[State] = initGrid(Empty())
  /*
   *  On the grid:
   *    - true means the tile is untouched.
   *    - false means somebody left a trace on it.
   */
  var coord: (Int, Int) = (0, 0)

  // game loop
  while(true) {
    // n: total number of players (2 to 4).
    // p: your player number (0 to 3).
    val Array(n, p) = for(i <- readLine split " ") yield i.toInt
    for(i <- 0 until n) {
      // x0: starting X coordinate of lightcycle (or -1)
      // y0: starting Y coordinate of lightcycle (or -1)
      // x1: starting X coordinate of lightcycle (can be the same as X0 if you play before this player)
      // y1: starting Y coordinate of lightcycle (can be the same as Y0 if you play before this player)
      val Array(x0, y0, x1, y1) = for(i <- readLine split " ") yield i.toInt

      // Leave trace on the board with the id of the lightcycle.
      setValAt(grid, Occupied(i), x0, y0)
      setValAt(grid, Occupied(i), x1, y1)

      if (i == p)
        coord = (x1, y1)
    }

    println(computeScore(grid, coord, p))
  }
}