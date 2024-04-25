package ex3

object Solitaire extends App:
  type Point2D = (Int, Int)
  type Solution = Vector[Point2D]
  type IterableFactory = Solution => Iterable[Solution]

  given IterableFactory = Vector(_)

  def render(solution: Solution, width: Int, height: Int): String =
    val rows =
      for
        y <- 0 until height
        row = for
          x <- 0 until width
          number = solution.indexOf((x, y)) + 1
        yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  def placeMarks(dim: Point2D)(using factory: IterableFactory): Iterable[Solution] =
    inline def makeMove(inline pos: Point2D, inline dir: Point2D): Point2D = (pos._1 + dir._1, pos._2 + dir._2)

    inline def isMoveLegal(inline move: Point2D): Boolean =
      move._1 >= 0 && move._2 >= 0 && move._1 < dim._1 && move._2 < dim._2

    val directions = Vector((3, 0), (0, 3), (-3, 0), (0, -3), (-2, -2), (2, -2), (2, 2), (-2, 2))
    val boardSize = dim._1 * dim._2
    def _placeMarks(currentPos: Point2D, currentSol: Solution): Iterable[Solution] = currentSol.length match
      case l if l == boardSize => factory(currentSol)
      case _ =>
        for
          dir <- directions
          move = makeMove(currentPos, dir)
          if isMoveLegal(move) && !currentSol.contains(move)
          sol <- _placeMarks(move, currentSol :+ move)
        yield sol

    val start = (dim._1 / 2, dim._2 / 2)
    _placeMarks(start, Vector(start))

  @main def main() =
    val dim = (5, 7)
    val solutions: Iterable[Solution] = placeMarks(dim)
    println(s"Solutions found: ${solutions.size}")
    println(s"First solution:")
    println(
      render(solution = solutions.headOption.fold(Vector.empty)(s => s), width = dim._1, height = dim._2)
    )
