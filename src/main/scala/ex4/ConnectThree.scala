package ex4

// Optional!
object ConnectThree extends App:
  val bound = 3
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

  private inline def maxBoardSize: Int =
    val boundInc = bound + 1
    boundInc * boundInc

  extension (b: Board)
    private def put(x: Int, player: Player): (Board, Boolean) = firstAvailableRow(b, x) match
      case None    => (b, false)
      case Some(y) => (Disk(x, y, player) +: b, true)

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
      yield if hasWon(player.other, g.head) then g else b +: g

  def hasWon(player: Player, board: Board): Boolean =
    inline def alignedHorizontally: (Disk, Disk) => Boolean = (d1, d2) => (d1.y == d2.y) && (d1.x == d2.x - 1)
    inline def alignedVertically: (Disk, Disk) => Boolean = (d1, d2) => (d1.x == d2.x) && (d1.y == d2.y - 1)
    inline def alignedDiagonally: (Disk, Disk) => Boolean = (d1, d2) => (d1.x == d2.x - 1) && (d1.y == d2.y - 1)

    val b = board.filter(_.player == player)
    (for
      d1 <- b
      d2 <- b
      d3 <- b
      disks = Array(d1, d2, d3)
      if (disks are alignedHorizontally) || (disks are alignedVertically) || (disks are alignedDiagonally)
    yield true).isDefinedAt(0)

  extension [T](l: Seq[T])
    private inline def are(pred: (T, T) => Boolean): Boolean = l.sliding(2).forall { case Seq(t1, t2) =>
      pred(t1, t2)
    }

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
      override def tick(board: Board): Board = Random.shuffle(placeAnyDisk(board, player)).head

    class SmartAI(player: Player) extends Controllable(player):
      override def tick(board: Board): Board = ???

    class Human(player: Player) extends Controllable(player):
      override def tick(board: Board): Board = ???

  import Controllables.*

  def play(player1: Controllable, player2: Controllable, onVictory: Player => Unit): LazyList[Game] =
    require(player1.player != player2.player, "Controllables should own different players.")

    def _play(p1: Controllable, p2: Controllable, moves: Int): LazyList[Game] = moves match
      case 0 => LazyList(newGame)
      case _ =>
        for
          g <- _play(p2, p1, moves - 1)
          p1Won = hasWon(p1.player, g.head)
          p2Won = hasWon(p2.player, g.head)
        yield
          if p1Won || p2Won then
            onVictory(if p1Won then p1.player else p2.player)
            g
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
  // val games = computeAnyGame(O, 6)
  // games.foreach { g =>
  //   printBoards(g)
  //   println()
  // }
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
  var winner: Option[Player] = None
  play(RandomAI(Player.O), RandomAI(Player.X), w => winner = Some(w)).foreach(printBoards)
  winner match
    case Some(player) => println(s"${player} has won!")
    case None         => println("Draw!")
