package ex4

// Optional!
object ConnectThree extends App:
  inline val bound = 3
  private inline val maxBoardSize = (bound + 1) * (bound + 1)

  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)
  /**
   * Board:
   * y
   *
   * 3
   * 2
   * 1
   * 0
   *   0 1 2 3 <-- x
   */
  type Board = Seq[Disk]
  type Game = Seq[Board]
  private val newGame: Game = Seq(Seq.empty)

  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] = board.collectFirst {
    case d if d.x == x && d.y == y => d.player
  }

  def firstAvailableRow(board: Board, x: Int): Option[Int] = board.filter(_.x == x) match
    case Nil      => Some(0)
    case b: Board => b.maxByOption(_.y).collect { case d if d.y < bound => d.y + 1 }

  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 to bound
      y <- firstAvailableRow(board, x)
    yield Disk(x, y, player) +: board

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match
    case 0 => LazyList(newGame)
    case _ =>
      for
        g <- computeAnyGame(player.other, moves - 1)
        b <- placeAnyDisk(g.head, player)
        w = g.head.winner
      yield if w.isDefined then g else b +: g

  extension (b: Board)
    private def put(x: Int, player: Player): (Board, Boolean) = firstAvailableRow(b, x) match
      case None    => (b, false)
      case Some(y) => (Disk(x, y, player) +: b, true)

    def winner: Option[Player] =
      inline def hasWon(player: Player): Boolean =
        val bf = b.filter(_.player == player)
        (for
          d1 <- bf
          d2 <- bf
          d3 <- bf
          if areAligned(Seq(d1, d2, d3))
        yield true).isDefinedAt(0)

      if hasWon(O) then return Some(O)
      if hasWon(X) then return Some(X)
      None

    def getUnoccupiedCellsNearby(disk: Disk): Seq[Disk] =
      (for
        x <- -1 to 1
        xn = clamp(disk.x + x, 0, bound)
        yn <- firstAvailableRow(b, xn)
        otherP = find(b, xn, yn)
        if otherP.isEmpty
      yield Disk(xn, yn, disk.player))

  inline def alignedHorizontally: (Disk, Disk) => Boolean = (d1, d2) => (d1.y == d2.y) && (d1.x == d2.x - 1)
  inline def alignedVertically: (Disk, Disk) => Boolean =   (d1, d2) => (d1.x == d2.x) && (d1.y == d2.y - 1)
  inline def alignedDiagonallyRight: (Disk, Disk) => Boolean = (d1, d2) => (d1.x == d2.x - 1) && (d1.y == d2.y - 1)
  inline def alignedDiagonallyLeft: (Disk, Disk) => Boolean =  (d1, d2) => (d1.x == d2.x + 1) && (d1.y == d2.y - 1)
  inline def areAligned: Seq[Disk] => Boolean = disks => 
    (disks are alignedHorizontally) || (disks are alignedVertically) || (disks are alignedDiagonallyRight) || (disks are alignedDiagonallyLeft)

  inline def clamp(v: Int, min: Int, max: Int): Int =
    if v < min then min
    else if v > max then max
    else v

  extension [T](l: Seq[T])
    private inline def are(pred: (T, T) => Boolean): Boolean = l.sliding(2).forall { case Seq(t1, t2) => pred(t1, t2) }

  def printBoards(game: Seq[Board]): Unit =
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()

  object Controllables:
    trait Controllable(val player: Player):
      def tick(board: Board): Board

    import scala.util.Random

    class RandomAI(player: Player) extends Controllable(player):
      override def tick(board: Board): Board =
        val bs = placeAnyDisk(board, player)
        bs(Random.nextInt(bs.length))

    class SmartAI(player: Player) extends Controllable(player):
      private val randomAI = RandomAI(player)

      override def tick(board: Board): Board =
        inline def getWinningCell(player: Player): Option[Disk] =
          (for
            b <- placeAnyDisk(board, player)
            w <- b.winner
            if w == player
          yield b.filterNot(board.toSet)).flatten.headOption

        val winningCell = getWinningCell(player)
        if winningCell.isDefined then return winningCell.get +: board

        val opponentWinningCell = getWinningCell(player.other)
        opponentWinningCell match
          case Some(d) => return Disk(d.x, d.y, player) +: board
          case _       =>

        val neighbors: Seq[Disk] = (for
          d1 <- board.filter(_.player == player)
          d2 <- board.getUnoccupiedCellsNearby(d1)
        yield d2).flatMap(d => Seq(d))
        if !neighbors.isEmpty then return neighbors(Random.nextInt(neighbors.length)) +: board

        return randomAI.tick(board)

    class Human(player: Player) extends Controllable(player):
      override def tick(board: Board): Board = ???

  import Controllables.*

  def play(player1: Controllable, player2: Controllable): LazyList[Game] =
    require(player1.player != player2.player, "Controllables should own different players.")

    def _play(p1: Controllable, p2: Controllable, moves: Int): LazyList[Game] = moves match
      case 0 => LazyList(newGame)
      case _ =>
        for
          g <- _play(p2, p1, moves - 1)
          w = g.head.winner
        yield
          if w.isDefined then g
          else p1.tick(g.head) +: g

    _play(player1, player2, maxBoardSize)

  // Exercise 1: implement find such that..
  println("EX 1: ")
  println(find(List(Disk(0, 0, X)), 0, 0)) // Some(X)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1)) // Some(O)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1)) // None

  // Exercise 2: implement firstAvailableRow such that..
  println("EX 2: ")
  println(firstAvailableRow(List(), 0)) // Some(0)
  println(firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None
  // Exercise 2: implement placeAnyDisk such that..
  printBoards(placeAnyDisk(List(), X))
  // .... .... .... ....
  // .... .... .... ....
  // .... .... .... ....
  // ...X ..X. .X.. X...
  printBoards(placeAnyDisk(List(Disk(0, 0, O)), X))
  // .... .... .... ....
  // .... .... .... ....
  // ...X .... .... ....
  // ...O ..XO .X.O X..O
  println("EX 4: ")
// Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  val games = computeAnyGame(O, 4)
  games.foreach { g =>
    printBoards(g)
    println()
  }
//  .... .... .... .... ...O
//  .... .... .... ...X ...X
//  .... .... ...O ...O ...O
//  .... ...X ...X ...X ...X
//
//
// .... .... .... .... O...
// .... .... .... X... X...
// .... .... O... O... O...
// .... X... X... X... X...

// Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!

// Exercise 5: AI
  println("Random AI:")
  val randomGame = play(RandomAI(O), RandomAI(X))
  randomGame.foreach(printBoards)
  randomGame.head.head.winner match
    case Some(player) => println(s"${player} has won!")
    case None         => println("Draw!")

  println("Smart AI:")
  val smartGame = play(SmartAI(O), SmartAI(X))
  smartGame.foreach(printBoards)
  smartGame.head.head.winner match
    case Some(player) => println(s"${player} has won!")
    case None         => println("Draw!")
