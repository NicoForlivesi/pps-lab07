package ex3

object Solitaire:
  object Position:
    type Position = (Int, Int)
    def apply(i: Int, j: Int): Position = (i, j)

  import Position.*
  type Solution = List[Position]

  object Measure:
    type Measure = Int
    def apply(n: Int): Measure =
      require(n >= 1)
      n

  import Measure.*
  case class Board(width: Measure, height: Measure)

  def placeMarks(board: Board): List[Solution] =
    val start = Position(board.width / 2, board.height / 2)

    def _place(current: Solution): List[Solution] =
      if current.size == board.width * board.height then List(current)
      else
        for
          next <- validMoves(current.head, current)
          solution <- _place(next :: current)
        yield solution

    def validMoves(pos: Position, current: Solution): List[Position] =
      val (x, y) = pos
      val next =
        List(
          (0, y), (board.width - 1, y),
          (x, 0), (x, board.height - 1),
          (x + 1, y + 1), (x + 1, y - 1),
          (x - 1, y + 1), (x - 1, y - 1)
        )
      for
        (nx, ny) <- next
        if 0 <= nx && nx < board.width
        if 0 <= ny && ny < board.height
        if !current.contains((nx, ny))
      yield (nx, ny)

    _place(List(start))

  def render(solution: Solution, width: Measure, height: Measure): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  @main
  def printSolutions(): Unit =
    val solutions = placeMarks(Board(3, 3))
    if solutions.isEmpty then
      println("No solution found")
    else
      solutions.foreach(s => println("Possible solution: \n" + render(s, 3, 3)))