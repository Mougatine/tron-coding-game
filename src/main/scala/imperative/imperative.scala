package imperative

 /*
  * login: <arthur.douillard@epita.fr>
  * password: <SalutAdrienCaVa?>
  * ranking: <12> // Classement des gens d'EPITA utilisant Scala
  */
object Player extends App {
  def width  = 30
  def height = 20

  sealed trait State
  case class Empty()               extends State
  case class Occupied(id: Int)     extends State
  case class Predicted(id: Int)    extends State

  type Grid = Array[State]

  abstract class Direction
  case class Up(x: Int, y: Int)   extends Direction
  case class Left(x: Int, y: Int)  extends Direction
  case class Down(x: Int, y: Int)  extends Direction
  case class Right(x: Int, y: Int) extends Direction

  def direction2string(direction: Direction): String = direction match {
    case Up(_, _)    => "UP"
    case Left(_, _)   => "LEFT"
    case Down(_, _)   => "DOWN"
    case Right(_, _)  => "RIGHT"
  }


  def direction2pos0(direction: Direction): (Int, Int) = direction match {
    case Up(x, y)    => (x, y)
    case Left(x, y)   => (x, y)
    case Down(x, y)   => (x, y)
    case Right(x, y)  => (x, y)
  }


  def direction2pos1(direction: Direction): (Int, Int) = direction match {
    case Up(x, y)    => (x, y - 1)
    case Left(x, y)   => (x - 1, y)
    case Down(x, y)   => (x, y + 1)
    case Right(x, y)  => (x + 1, y)
  }


  def printGrid(grid: Grid): Unit = {
    def prettyPrint(state: State): String = state match {
      case Empty()        => "."
      case Occupied(id)   => id.toString
      case Predicted(id)  => id.toString
    }

    for (y <- 0 until height) {
      for (x <- 0 until width) {
        System.err.print(prettyPrint(getValAt(grid, x, y)))
      }
      System.err.println()
    }
    System.err.println()
  }


  def setDead(grid: Grid, id: Int): Unit = {
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        if (getValAt(grid, x, y) == Occupied(id))
          setValAt(grid, Empty(), x, y)
      }
    }
  }


  def getValAt(grid: Grid, x: Int, y: Int): State =
    grid(x + y * width)


  def setValAt(grid: Grid, va: State, x: Int, y: Int): Unit =
    grid(x + y * width) = va


  def initGrid(): Grid =
    Array.fill[State](width * height)(Empty())


  def getNeighbours(fun: (Grid, Int, Int) => Boolean)(grid: Grid, x: Int, y: Int): Array[(Int, Int)] = {
    def possibleMoves: Array[(Int, Int)] =
      Array((0, -1), (-1, 0), (0, 1), (1, 0))

    def inGrid(x: Int, y: Int) =
      x >= 0 && x < width && y >= 0 && y < height

    def cond(x: Int, y: Int): Boolean =
      inGrid(x, y) && fun(grid, x, y)

    possibleMoves
      .map(fact => (x + fact._1, y + fact._2))
      .filter(coord => cond(coord._1, coord._2))
  }


  def isValidTile(grid: Grid, x: Int, y: Int): Boolean =
    x >= 0 && x < width && y >= 0 && y < height && getValAt(grid, x, y) == Empty()


  /*
   *  Voronoi algorithm:
   *    While there are still reachable tiles, for each player turn, expand to
   *    all adjacent tiles of last positions.
   */
  def voronoi(grid: Grid, lastMovesAll: Array[Array[(Int, Int)]], nbPlayers: Int, me: Int): Grid = {
    // All false when every reachable state for each player have been reached.
    val flags = Array.fill(nbPlayers)(true)
    var flag = true
    while (flags.contains(true)) {
      for (playerId <- (me + 1 until nbPlayers).toList ++ (0 to me).toList) {
        if (flag && playerId == me)
          flag = false
        else {
          lastMovesAll(playerId) = playTurn(grid, lastMovesAll(playerId), Predicted(playerId))
          if (lastMovesAll(playerId).isEmpty)
            flags(playerId) = false
        }
      }
    }

    grid
  }


  /*
   *  Expand for one player (i.e. one turn) to all adjacent tiles of the last
   *  positions.
   */
  def playTurn(grid: Grid, lastMovesPlayer: Array[(Int, Int)], state: State): Array[(Int, Int)] = {
    def emptyCond = (grid: Grid, x: Int, y: Int) => getValAt(grid, x, y) == Empty()

    val nextMoves = lastMovesPlayer
      .flatMap(move => getNeighbours(emptyCond)(grid, move._1, move._2))
      .distinct

    nextMoves.foreach(move => setValAt(grid, state, move._1, move._2))
    nextMoves
  }


  def isAlone(grid: Grid, id: Int): Boolean = {
    val predictCond =
      (grid: Grid, x: Int, y: Int) => getValAt(grid, x, y) == Predicted(id)
    val myPrediction =
      (state: State) => state == Predicted(id)
    val opPrediction =
      (state: State) => state.isInstanceOf[Predicted] && !myPrediction(state)

    var acc = 0
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        if (opPrediction(getValAt(grid, x, y))) {
          acc += getNeighbours(predictCond)(grid, x, y).length
        }
      }
    }

    acc == 0
  }


  def computeScore(grid: Grid, id: Int): Int = {
    val myPrediction =
      (state: State) => state == Predicted(id)
    val opPrediction =
      (state: State) => state.isInstanceOf[Predicted] && !myPrediction(state)

    val myTiles = grid.count(tile => myPrediction(tile))
    val opTiles = grid.count(tile => opPrediction(tile))

    10 * myTiles - 1 * opTiles
  }


  def getNextMove(grid: Grid, lastMovesAll: Array[Array[(Int, Int)]], nbPlayers: Int, me: Int): String = {
    def wallHugger(grid: Grid, direction: Direction): Int = {
      val (x, y) = direction2pos1(direction)
      setValAt(grid, Occupied(me), x, y)
      val wallCond = (grid: Grid, x1: Int, y1: Int) => getValAt(grid, x1, y1).isInstanceOf[Occupied]

      val wallNumber = getNeighbours(wallCond)(grid, x, y).length

      System.err.println("Wall:", direction2string(direction), wallNumber)

      wallNumber match {
        case 0 => 0
        case 1 => 0
        case 2 => 100
        case 3 => 60
      }
    }

    def goDirection(grid: Grid, direction: Direction, moves: Array[Array[(Int, Int)]]): Int = {
      val (newX, newY) = direction2pos1(direction)
      setValAt(grid, Predicted(me), newX, newY)
      moves(me) = Array((newX, newY))

      val ansGrid = voronoi(grid, moves, nbPlayers, me)
      //printGrid(ansGrid)
      computeScore(ansGrid, me)
    }

    def goFourDirections(grid: Grid, direction: Direction, moves: Array[Array[(Int, Int)]]): Int = {
      val (newX, newY) = direction2pos1(direction)
      setValAt(grid, Predicted(me), newX, newY)
      moves(me) = Array((newX, newY))

      //for (playerId <- (me + 1 until nbPlayers).toList ++ (0 until me).toList)
      //  moves(playerId) = playTurn(grid, moves(playerId), Predicted(playerId))

      val ans = Array(Up(newX, newY), Left(newX, newY), Down(newX, newY), Right(newX, newY))
        .filter(dir => isValidTile(grid, direction2pos1(dir)._1, direction2pos1(dir)._2))
        .map(dir => goDirection(grid.clone, dir, lastMovesAll.clone))

      if (ans.isEmpty)
        -100
      else
        ans.max
    }

    def lookup(fun: (Grid, Direction, Array[Array[(Int, Int)]]) => Int)
              (grid: Grid, moves: Array[Array[(Int, Int)]]): String = {
      val (x, y) = moves(me)(0)
      val directions = Array(Up(x, y), Left(x, y), Down(x, y), Right(x, y))
        .filter(dir => isValidTile(grid, direction2pos1(dir)._1, direction2pos1(dir)._2))
        .map(dir => (fun(grid.clone, dir, lastMovesAll.clone), dir))


      if (directions.isEmpty)
        "Oh shit..."
      else {
        directions.foreach(x => System.err.println(direction2string(x._2), x._1))
        val maxVal = directions.maxBy(_._1)._1
        val ans = directions
            .filter(x => x._1 == maxVal)
            .map(x => (wallHugger(grid, x._2), x._2))
        
        direction2string(ans.maxBy(_._1)._2)
      }
    }

    lookup(goFourDirections)(grid.clone, lastMovesAll.clone)
  }


  val grid: Grid = initGrid()

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
    println(getNextMove(grid, currentMoves, n, p))
    System.err.println("Time:", System.currentTimeMillis() - now)
  }
}

