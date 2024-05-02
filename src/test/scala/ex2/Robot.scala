package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotSpec extends AnyFlatSpec with Matchers:
  "A SimpleRobot" should "turn correctly" in:
    val robot = new SimpleRobot((0, 0), Direction.North)

    robot.turn(Direction.East)
    robot.direction should be(Direction.East)

    robot.turn(Direction.South)
    robot.direction should be(Direction.South)

    robot.turn(Direction.West)
    robot.direction should be(Direction.West)

    robot.turn(Direction.North)
    robot.direction should be(Direction.North)

  it should "act correctly" in:
    val robot = new SimpleRobot((0, 0), Direction.North)

    robot.act()
    robot.position should be((0, 1))

    robot.turn(Direction.East)
    robot.act()
    robot.position should be((1, 1))

    robot.turn(Direction.South)
    robot.act()
    robot.position should be((1, 0))

    robot.turn(Direction.West)
    robot.act()
    robot.position should be((0, 0))
  
  "A RobotWithBattery" should "turn only if the battery level is high enough" in:
    val robot = RobotWithBattery(SimpleRobot((0, 0), Direction.North), 100.0, 50.0)

    robot.turn(Direction.East)
    robot.direction should be(Direction.East)

    robot.turn(Direction.South)
    robot.direction should be(Direction.South)

    robot.turn(Direction.West)
    robot.direction should be(Direction.South)

  it should "act only if the battery level is high enough" in:
    val robot = RobotWithBattery(SimpleRobot((0, 0), Direction.North), 100.0, 40.0)

    robot.act()
    robot.position should be((0, 1))

    robot.turn(Direction.East)
    robot.act()
    robot.position should be((1, 1))

    robot.act()
    robot.position should be((1, 1))

  "A RobotCanFail" should "turn correctly" in:
    val seed = 42
    val robot = RobotCanFail(SimpleRobot((0, 0), Direction.North), 0.5, scala.util.Random(seed))

    robot.turn(Direction.East)
    robot.direction should be(Direction.East)

    robot.turn(Direction.South)
    robot.direction should be(Direction.South)

    robot.turn(Direction.West)
    robot.direction should be(Direction.South)

    robot.turn(Direction.West)
    robot.turn(Direction.West)
    robot.direction should be(Direction.West)

  it should "act correctly" in:
    val seed = 42
    val robot = RobotCanFail(SimpleRobot((0, 0), Direction.North), 0.5, scala.util.Random(seed))

    robot.act()
    robot.position should be((0, 1))

    robot.turn(Direction.East)
    robot.act()
    robot.position should be((0, 1))

    robot.act()
    robot.act()
    robot.position should be((1, 1))

  "A RobotRepeated" should "keep turning" in:
    val robot = RobotRepeated(SimpleRobot((0, 0), Direction.North), 2)

    robot.turn(Direction.East)
    robot.direction should be(Direction.West)

    robot.turn(Direction.South)
    robot.direction should be(Direction.North)

    robot.turn(Direction.North)
    robot.direction should be(Direction.North)

  it should "keep executing its action" in:
    val robot = RobotRepeated(SimpleRobot((0, 0), Direction.North), 2)

    robot.act()
    robot.position should be((0, 3))

    robot.turn(Direction.East)
    robot.act()
    robot.position should be((-3, 3))