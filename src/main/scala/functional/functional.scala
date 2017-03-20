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


  def initGrid(default: State): Array[State] =
    Array.fill(width * height)(default)


  /*
   *  Returns all valid neighbours given a coordinates.
   *  Is valid if the tile is in the inner bounds of the map and the tile
   *  is hosting an Empty() case class instance.
   */
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


  /*
   *  Compute the best direction to take between UP, LEFT, DOWN, and RIGHT.
   *  grid: The current grid representing the global state of the game.
   *  coord: The coordinates of my last position.
   *  me: My id.
   */
  def computeScore(grid: Array[State], coord: (Int, Int), me: Int): String = {
    /*
     *  Return all possible moves change. i.e. the difference of coordinates to make a move.
     *  It computes the moves up to two turns in advance.
     */
    val movesAround: Array[(Int, Int)] = {
      def firstStep: Array[(Int, Int)] = Array((0, -1), (-1, 0), (0, 1), (1, 0))

      firstStep
        .flatMap(x => firstStep.map(y => (x._1 + y._1, x._2 + y._2)))
        .filter(x => x != (0, 0))
        .distinct ++ firstStep
    }


    /*
     *  Check if a tile, given its coordinates, is correct.
     */
    def validTile(grid: Array[State], x: Int, y: Int): Boolean =
      x >= 0 && x < width && y >= 0 && y < height && getValAt(grid, x, y) == Empty()

    /*
     *  Compute score.
     *  The more valid tile are around me, the better the score is.
     */
    def getScore(grid: Array[State], x: Int, y: Int): Int = {
      movesAround.map(move => (x + move._1, y + move._2))
        .count(move => validTile(grid, move._1, move._2))
    }

    /*
     *  Go in all four direction.
     */
    def allMoves(grid: Array[State], x: Int, y: Int, depth: Int): Array[Int] =
      Array(helper(grid.clone, x, y - 1, depth),
        helper(grid.clone, x - 1, y, depth),
        helper(grid.clone, x, y + 1, depth),
        helper(grid.clone, x + 1, y, depth))

    /*
     *  Predict best score depending of my future moves for 3 turns in advance.
     */
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
  val coord: Array[(Int, Int)] = Array((0, 0))

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
        coord(0) = (x1, y1)
    }

    println(computeScore(grid, coord(0), p))
  }
}