import math._
import scala.util._

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player extends App {
  def width  = 30
  def height = 20


  def getValAt(grid: Array[Int], x: Int, y: Int): Int = {
    grid(x + y * width)
  }

  def setValAt(grid: Array[Int], va: Int, x: Int, y: Int): Unit =
    if (x != -1 || y != -1)
      grid(x + y * width) = va


  def initGrid(default: Int): Array[Int] = {
    def fillLine: Array[Int] =
      Array.fill(width)(default)

    def fillGrid(y: Int, grid: Array[Int]): Array[Int] = {
      if (y >= height)
        grid
      else
        fillGrid(y + 1, grid ++ fillLine)
    }

    fillGrid(0, Array[Int]())
  }

  def getValidNeighbours(grid: Array[Int], x: Int, y: Int): Array[(Int, Int)] = {
    def possibleMoves: Array[(Int, Int)] =
      Array((0, -1), (-1, 0), (0, 1), (1, 0))

    def inGrid(x: Int, y: Int) =
      x >= 0 && x < width && y >= 0 && y < height

    def cond(x: Int, y: Int): Boolean =
      inGrid(x, y)  && getValAt(grid, x, y) == -2

    possibleMoves
      .map(fact => (x + fact._1, y + fact._2))
      .filter(coord => cond(coord._1, coord._2))
  }


  def computeScore(grid: Array[Int], coord: (Int, Int), me: Int): String = {
    def endCond(grid: Array[Int]): Boolean = !grid.contains(-2)

    def getScore(grid: Array[Int]): Int = grid.count(_ == me)

    def allMoves(grid: Array[Int], x: Int, y: Int, depth: Int): Array[Int] =
      Array(helper(grid.clone, x, y - 1, depth),
        helper(grid.clone, x - 1, y, depth),
        helper(grid.clone, x, y + 1, depth),
        helper(grid.clone, x + 1, y, depth))

    def helper(grid: Array[Int], x: Int, y: Int, depth: Int): Int = {
      if (x < 0 || x >= width || y < 0 || y >= height || getValAt(grid, x, y) != -2
        || getValidNeighbours(grid, x, y).size <= 1)
        -10000000
      else if (depth >= 7)
        getScore(grid)
      else {
        setValAt(grid, me, x, y)
        allMoves(grid, x, y, depth + 1).max
      }
    }

    val x = coord._1
    val y = coord._2
    Array((helper(grid.clone, x, y - 1, 0), "UP"),
      (helper(grid.clone, x - 1, y, 0), "LEFT"),
      (helper(grid.clone, x, y + 1, 0), "DOWN"),
      (helper(grid.clone, x + 1, y, 0), "RIGHT")).maxBy(_._1)._2
  }

  val grid: Array[Int] = initGrid(-2)
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
      setValAt(grid, i, x0, y0)
      setValAt(grid, i, x1, y1)

      if (i == p)
        coord = (x1, y1)
    }

    val t0 = System.currentTimeMillis()
    println(computeScore(grid, coord, p))
    System.err.println(System.currentTimeMillis() - t0)
  }
}