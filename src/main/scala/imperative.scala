import math._
import scala.util._

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player extends App {
  def width  = 30
  def height = 20

  sealed trait State
  case class Empty()            extends State
  case class Occupied(id: Int)  extends State
  case class Predicted(id: Int) extends State

  def printGrid(grid: Array[State]): Unit = {
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        System.err.print(getValAt(grid, x, y).toString + " ")
      }
      System.err.println()
    }
  }

  def setDead(grid: Array[State], id: Int): Unit = {
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        if (getValAt(grid, x, y) == Occupied(id))
          setValAt(grid, Empty(), x, y)
      }
    }
  }

  def getValAt(grid: Array[State], x: Int, y: Int): State = {
    grid(x + y * width)
  }

  def setValAt(grid: Array[State], va: State, x: Int, y: Int): Unit =
    grid(x + y * width) = va


  def initGrid(): Array[State] = {
    def fillLine: Array[State] =
      Array.fill(width)(Empty())

    def fillGrid(y: Int, grid: Array[State]): Array[State] = {
      if (y >= height)
        grid
      else
        fillGrid(y + 1, grid ++ fillLine)
    }

    fillGrid(0, Array())
  }

  def getValidNeighbours(grid: Array[State], x: Int, y: Int): Array[(Int, Int)] = {
    def possibleMoves: Array[(Int, Int)] =
      Array((0, -1), (-1, 0), (0, 1), (1, 0))

    def inGrid(x: Int, y: Int) =
      x >= 0 && x < width && y >= 0 && y < height

    def cond(x: Int, y: Int): Boolean =
      inGrid(x, y) && getValAt(grid, x, y) == Empty()

    possibleMoves
      .map(fact => (x + fact._1, y + fact._2))
      .filter(coord => cond(coord._1, coord._2))
  }


  def nextMove(grid: Array[State], currentMoves: Array[Array[(Int, Int)]], me: Int, players: Int): String = {
    def computeScore(grid: Array[State], id: Int): Int = {
      val myTiles = grid.count(tile => tile == Predicted(id))
      val opTiles = grid.count(tile => tile.isInstanceOf[Predicted]) - myTiles
      var possibleMoves: Seq[(Int, Int)] = Seq()

      for (y <- 0 until height) {
        for (x <- 0 until width) {
          if (getValAt(grid, x, y) == Predicted(id)) {
            possibleMoves = possibleMoves ++ getValidNeighbours(grid, x, y)
          }
        }
      }

      10000 * myTiles - 10 * opTiles - possibleMoves.length
    }

    def validTile(grid: Array[State], x: Int, y: Int): Boolean =
      x >= 0 && x < width && y >= 0 && y < height && getValAt(grid, x, y) == Empty()

    def helper(grid: Array[State], currMoves: Array[Array[(Int, Int)]]): Array[State] = {
      var flags = Array.fill(players)(true)
      while (flags.contains(true)) {
        for (playerId <- 0 until players) {
          currMoves(playerId) = playTurn(grid, currMoves(playerId), playerId)
          if (currMoves(playerId).isEmpty)
            flags(playerId) = false
        }
      }

      grid
    }

    def playTurn(grid: Array[State], currMoves: Array[(Int, Int)], playerId: Int): Array[(Int, Int)] = {
      val nextMoves = currMoves
        .flatMap(move => getValidNeighbours(grid, move._1, move._2))
        .distinct

      nextMoves.foreach(move => setValAt(grid, Predicted(playerId), move._1, move._2))
      nextMoves
    }

    def goDirection(grid: Array[State], currMoves: Array[Array[(Int, Int)]],
                    x: Int, y: Int): Option[Array[State]] = {
      if (!validTile(grid, x, y))
        None
      else {
        currMoves(me) = Array((x, y))
        Some(helper(grid, currMoves))
      }
    }

    def goDirections(grid: Array[State], currMoves: Array[Array[(Int, Int)]],
                     x: Int, y: Int): String = {
      Array((goDirection(grid.clone, currentMoves.clone, x, y - 1), "UP"),
        (goDirection(grid.clone, currentMoves.clone, x - 1, y), "LEFT"),
        (goDirection(grid.clone, currentMoves.clone, x, y + 1), "DOWN"),
        (goDirection(grid.clone, currentMoves.clone, x + 1, y), "RIGHT"))
        .filter(tuple => tuple._1.isDefined)
        .map(tuple => (tuple._1.get, tuple._2))
        .map(tuple => (computeScore(tuple._1, me), tuple._2))
        .maxBy(_._1)._2
    }

    val x = currentMoves(me)(0)._1
    val y = currentMoves(me)(0)._2
    goDirections(grid, currentMoves, x, y)
  }


  val grid: Array[State] = initGrid()

  // game loop
  while(true) {
    // n: total number of players (2 to 4).
    // p: your player number (0 to 3).
    val Array(n, p) = for(i <- readLine split " ") yield i.toInt
    var currentMoves: Array[Array[(Int, Int)]] = Array.fill(n)(Array[(Int, Int)]())

    for(i <- 0 until n) {
      // x0: starting X coordinate of lightcycle (or -1)
      // y0: starting Y coordinate of lightcycle (or -1)
      // x1: starting X coordinate of lightcycle (can be the same as X0 if you play before this player)
      // y1: starting Y coordinate of lightcycle (can be the same as Y0 if you play before this player)
      val Array(x0, y0, x1, y1) = for(i <- readLine split " ") yield i.toInt

      // Leave trace on the board with the id of the lightcycle.
      if (x0 == -1)
        setDead(grid, i)
      else {
        setValAt(grid, Occupied(i), x0, y0)
        setValAt(grid, Occupied(i), x1, y1)
        currentMoves(i) = Array((x1, y1))
      }
    }

    val now = System.currentTimeMillis()
    println(nextMove(grid, currentMoves, p, n))
    System.err.println("Time:", System.currentTimeMillis() - now)
  }
}
