package ex3

object Solitaire extends App:
  type Point2D = (Int, Int)
  type Solution = Seq[Point2D]
  type SolutionFactory = Point2D => Solution
  type IterableFactory = Solution => Iterable[Solution]

  given SolutionFactory = List(_) 
  given IterableFactory = LazyList(_)

  extension (p: Point2D)
    inline def add(p2: Point2D): Point2D = (p._1 + p2._1, p._2 + p2._2)

  def placeMarks(width: Int, height: Int)(using iterableFactory: IterableFactory, solFactory: SolutionFactory): Iterable[Solution] =
    inline def isMoveLegal(m: Point2D): Boolean = m._1 >= 0 && m._2 >= 0 && m._1 < width && m._2 < height 

    val start = (width / 2, height / 2)
    val directions = Array((3, 0), (0, 3), (-3, 0), (0, -3), (-2, -2), (2, -2), (2, 2), (-2, 2))
    def _placeMarks(n: Int): Iterable[Solution] = n match
      case 0 => iterableFactory(solFactory(start))
      case _ =>
        for
          sol <- _placeMarks(n - 1)
          dir <- directions
          move = sol.head add dir
          if isMoveLegal(move) && !(sol contains move)
        yield 
          move +: sol

    _placeMarks(width * height - 1)

  def render(solution: Solution, width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for
        y <- 0 until height
        row = for
          x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
        yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  val width = 5
  val height = 7
  val solutions: Iterable[Solution] = placeMarks(width, height)
  println(s"Solutions found: ${solutions.size}")
  println(s"First solution:")
  println(
    render(solution = solutions.headOption.getOrElse(Seq.empty), width, height)
  )
