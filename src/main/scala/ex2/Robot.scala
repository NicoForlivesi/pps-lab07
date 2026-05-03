package ex2

type Position = (Int, Int)

object PercentValue:
  opaque type PercentValue = Int

  def apply(n: Int): PercentValue =
    require(n >= 0 && n <= 100)
    n

  extension(b: PercentValue)
    def value: Int = b
    def decrease(n: Int): PercentValue = b - n
    def toProb: Double = b / 100.0

object RepeatNTimes:
  opaque type RepeatNTimes = Int

  def apply(n: Int): RepeatNTimes =
    require(n >= 0)
    n

  extension(b: RepeatNTimes)
    def value: Int = b
    def subOne(): RepeatNTimes = b - 1

enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

import PercentValue.*
class RobotWithBattery(val robot: Robot, private var battery: PercentValue) extends Robot:
  export robot.{position, direction, turn}
  private final val decreaseAmount = 10;
  override def act(): Unit = battery.value - decreaseAmount match
    case k if k < 0 => println("Battery too low")
    case _ =>
      battery = battery.decrease(decreaseAmount)
      robot.act()

class RobotCanFail(val robot: Robot, private var failChance: PercentValue) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit = if Math.random() >= failChance.toProb then robot.act() else println("Action failed due to probability")

import RepeatNTimes.*
class RobotRepeated(val robot: Robot, private var timeToRepeat: RepeatNTimes) extends Robot:
  export robot.{position, direction, turn}
  private val n = timeToRepeat.value
  override def act(): Unit = timeToRepeat.value match
    case 0 => timeToRepeat = RepeatNTimes(n)
    case _ =>
      timeToRepeat = timeToRepeat.subOne()
      robot.act()
      act()

@main def testRobot(): Unit =
  val robotLogger = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  val robot = RobotWithBattery(robotLogger, PercentValue(30))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East
  robot.act() // Battery too low

  val robot2 = RobotCanFail(robotLogger, PercentValue(50))
  println("\nRobotCanFail: ")
  robot2.act()
  robot2.act()
  robot2.act()
  robot2.act()

  val robot3 = RobotRepeated(robotLogger, RepeatNTimes(3))
  println("\nRobotRepeated: ")
  robot3.act()
  robot3.act()
