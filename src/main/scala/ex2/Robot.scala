package ex2

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East  => Direction.South
    case Direction.South => Direction.West
    case Direction.West  => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West  => Direction.South
    case Direction.South => Direction.East
    case Direction.East  => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East  => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West  => (position._1 - 1, position._2)

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

trait FailableAction[R]:
  protected def canExecute: Boolean
  protected def tryExecute(action: => R, orElse: => R): R = if canExecute then action else orElse

class RobotWithBattery(val robot: Robot, val batteryCapacity: Double, val consumption: Double)
    extends Robot
    with FailableAction[Unit]:
  export robot.{position, direction}
  require(batteryCapacity > 0.0, "Capacity should be a positive value.")
  require(
    0.0 <= consumption && consumption <= batteryCapacity,
    "Battery consumption should be a value between 0 and capacity."
  )

  private var currentBattery = batteryCapacity

  private def drainBattery() = currentBattery = math.max(currentBattery - consumption, 0.0)
  override protected def canExecute: Boolean = currentBattery > 0.0
  override protected def tryExecute(action: => Unit, orElse: => Unit = None) =
    if canExecute then
      drainBattery()
      action
    else orElse

  override def turn(dir: Direction): Unit = tryExecute(robot.turn(dir))
  override def act(): Unit = tryExecute(robot.act())
  override def toString(): String = s"${robot.toString()} (battery: ${currentBattery})"

class RobotCanFail(val robot: Robot, val failureChance: Double) extends Robot with FailableAction[Unit]:
  export robot.{position, direction}
  require(0.0 <= failureChance && failureChance <= 1.0, "Failure chance should be a value between 0.0 and 1.0.")

  override protected def canExecute: Boolean = failureChance == 0.0 || math.random() >= failureChance

  override def turn(dir: Direction): Unit = super.tryExecute(robot.turn(dir), println("failed to turn!"))
  override def act(): Unit = super.tryExecute(robot.act(), println("failed to act!"))
  override def toString(): String = robot.toString()

class RobotRepeated(val robot: Robot, val repeatCount: Int) extends Robot:
  export robot.{position, direction, turn}
  require(repeatCount >= 0, "Repeats count should not be negative.")

  private def repeat(action: => Unit) = for _ <- 0 to repeatCount do action
  override def act(): Unit = repeat(robot.act())
  override def toString(): String = robot.toString()

@main def testRobot(): Unit =
  val robot = LoggingRobot(
    RobotWithBattery(SimpleRobot((0, 0), Direction.North), batteryCapacity = 100, consumption = 50)
  )
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (0, 1) facing East
  robot.act() // robot at (0, 1) facing East

  val robotCF = LoggingRobot(RobotCanFail(SimpleRobot((0, 0), Direction.North), failureChance = 0.5))
  robotCF.act()
  robotCF.turn(robotCF.direction.turnRight)
  robotCF.act()
  robotCF.act()

  val robotR = LoggingRobot(RobotRepeated(SimpleRobot((0, 0), Direction.North), repeatCount = 2))
  robotR.act() // robot at (0, 3) facing North
  robotR.turn(robotR.direction.turnRight)
  robotR.act() // robot at (3, 3) facing East
  robotR.act() // robot at (6, 3) facing East
