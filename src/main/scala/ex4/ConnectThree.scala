package ex4

import java.util.OptionalInt
import ex3.Solitaire.width

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
  type Game = Seq[Board]

  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] = board.collectFirst {
    case d if d.x == x && d.y == y => d.player
  }

  def firstAvailableRow(board: Board, x: Int): Option[Int] = board match
    case Nil => Some(0)
    case _ =>
      board
        .filter(_.x == x)
        .sortWith(_.y > _.y)
        .headOption
        .collect { case d if d.y < bound => d.y + 1 }

  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 to bound
      y = firstAvailableRow(board, x).getOrElse(0)
    yield
      Disk(x, y, player) +: board

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] =
    def hasPlayerWon(player: Player, board: Board): Boolean =
      inline def alignedHorizontally(d1: Disk, d2: Disk, d3: Disk): Boolean = (d1.y == d2.y && d2.y == d3.y) && (d1.x == d2.x - 1) && (d2.x == d3.x - 1)
      inline def alignedVertically(d1: Disk, d2: Disk, d3: Disk): Boolean =   (d1.x == d2.x && d2.x == d3.x) && (d1.y == d2.y - 1) && (d2.y == d3.y - 1)
      inline def alignedDiagonally(d1: Disk, d2: Disk, d3: Disk): Boolean = (d1.x == d2.x - 1 && d1.y == d2.y - 1) && (d2.x == d3.x - 1 && d2.y == d3.y - 1)
      inline def areAligned(d1: Disk, d2: Disk, d3: Disk): Boolean =
        alignedHorizontally(d1, d2, d3) || alignedVertically(d1, d2, d3) || alignedDiagonally(d1, d2, d3)

      val b = board.filter(_.player == player)
      (for
        d1 <- b 
        d2 <- b 
        d3 <- b
        if d1 != d2 && d1 != d3 && d2 != d3 && areAligned(d1, d2, d3)
      yield
        true).headOption.isDefined

    def compute(player: Player, moves: Int, game: Game): Iterable[Game] = moves match
      case 0 => Seq(game)
      case _ =>
        for
          b <- placeAnyDisk(game.headOption.getOrElse(Seq.empty), player)
          if !hasPlayerWon(player, b)
          g <- compute(player.other, moves - 1, b +: game)
        yield g

    compute(player, moves, Seq.empty).to(LazyList)
    
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
  val games = computeAnyGame(O, 5)
  games.foreach { g =>
    printBoards(g)
    println()
  }
  println(games.length)
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
