package ex3

object Solitaire extends App:
  object Position:
    type Position = (Int, Int)
    def apply(i: Int, j: Int): Position =
      require(i >= 0 && j >= 0)
      (i, j)

  import Position.*
  type Solution = List[Position]

  object Measure:
    type Measure = Int
    def apply(n: Int): Measure =
      require(n >= 1)
      n

  import Measure.*
  case class Board(width: Measure, height: Measure)

  def placeMarks(board: Board): LazyList[Solution] =
    val start = Position(board.width / 2, board.height / 2)

    def _place(current: Solution): LazyList[Solution] =
      if current.size == board.width * board.height then LazyList(current)
      else
        for
          next <- validMoves(current.head, current).to(LazyList)
          solution <- _place(next :: current)
        yield solution

    def validMoves(pos: Position, current: Solution): List[Position] =
      val offsets = List((2, 0), (-2, 0), (0, 2), (0, -2), (1, 1), (1, -1), (-1, 1), (-1, -1))
      for
        (dx, dy) <- offsets
        newX = pos._1 + dx
        newY = pos._2 + dy
        if newX >= 0 && newX < board.width
        if newY >= 0 && newY < board.height
        newPos = (newX, newY)
        if !current.contains(newPos)
      yield newPos

    _place(List(start))

  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")


  println(render(solution = Seq((0, 0), (2, 1)), width = 3, height = 3))

  @main
  def printSolutions(): Unit =
    val solutions = placeMarks(Board(5, 5)).foreach(s =>
      println("Possible solution: \n" + render(s, 5, 5)))