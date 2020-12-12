package aoc

import aoc.util.Resources

object Day12 extends App {

  sealed trait Instruction extends Product with Serializable
  object Instruction {
    case class Forward(by: Int) extends Instruction
    case class Move(direction: Direction, by: Int) extends Instruction
    case class Rotate90Degrees(direction: RotationDirection, times: Int) extends Instruction
  }

  sealed trait Direction extends Product with Serializable {
    def asIncrement(by: Int): Increment = this match {
      case Direction.East => Increment(dx = by)
      case Direction.South => Increment(dy = -by)
      case Direction.West => Increment(dx = -by)
      case Direction.North => Increment(dy = by)
    }
  }

  object Direction {
    case object East extends Direction
    case object South extends Direction
    case object West extends Direction
    case object North extends Direction
  }

  sealed trait RotationDirection extends Product with Serializable
  object RotationDirection {
    case object Left extends RotationDirection
    case object Right extends RotationDirection
  }

  case class Coordinate(x: Int = 0, y: Int = 0) {
    def move(increment: Increment): Coordinate = Coordinate(x + increment.dx, y + increment.dy)
    def manhattanDistance(to: Coordinate): Int = (to.x - x).abs + (to.y - y).abs
  }

  case class Increment(dx: Int = 0, dy: Int = 0) {
    def +(increment: Increment): Increment = Increment(dx + increment.dx, dy + increment.dy)
    def *(times: Int): Increment = Increment(dx * times, dy * times)

    def rotate90Right: Increment = Increment(dy, -dx)
    def rotate90Left: Increment = Increment(-dy, dx)
    def rotate180: Increment = Increment(-dx, -dy)

    def rotate(direction: RotationDirection, times: Int): Increment = {
      val timesMod = times % 4
      if (timesMod == 0) this
      else direction match {
        case RotationDirection.Right => times match {
          case 1 => rotate90Right
          case 2 => rotate180
          case 3 => rotate90Left
        }
        case RotationDirection.Left => times match {
          case 1 => rotate90Left
          case 2 => rotate180
          case 3 => rotate90Right
        }
      }
    }
  }

  case class Ship(waypoint: Increment, position: Coordinate = Coordinate()) {
    def move(direction: Direction, by: Int): Ship =
      copy(position = position.move(direction.asIncrement(by)))
    def moveToWaypoint(times: Int): Ship =
      copy(position = position.move(waypoint * times))
    def moveWaypoint(direction: Direction, by: Int): Ship =
      copy(waypoint = waypoint + direction.asIncrement(by))
    def rotate(direction: RotationDirection, times: Int): Ship =
      copy(waypoint = waypoint.rotate(direction, times))
  }

  def solvePart1(instructions: List[Instruction]) = {
    val ship0 = Ship(waypoint = Increment(1)) // east direction
    val ship1 = instructions.foldLeft(ship0)(executeInstruction(_.move))

    ship0.position.manhattanDistance(ship1.position)
  }

  def solvePart2(instructions: List[Instruction]) = {
    val ship0 = Ship(waypoint = Increment(10, 1))
    val ship1 = instructions.foldLeft(ship0)(executeInstruction(_.moveWaypoint))

    ship0.position.manhattanDistance(ship1.position)
  }

  private def executeInstruction(move: Ship => (Direction, Int) => Ship)(ship: Ship, instruction: Instruction) =
    instruction match {
      case Instruction.Forward(by) => ship.moveToWaypoint(by)
      case Instruction.Move(direction, by) => move(ship)(direction, by)
      case Instruction.Rotate90Degrees(direction, times) => ship.rotate(direction, times)
    }

  private def parseInstruction(s: String) = s match {
    case s"N$by" => Instruction.Move(Direction.North, by.toInt)
    case s"S$by" => Instruction.Move(Direction.South, by.toInt)
    case s"E$by" => Instruction.Move(Direction.East, by.toInt)
    case s"W$by" => Instruction.Move(Direction.West, by.toInt)
    case s"L$degrees" => Instruction.Rotate90Degrees(RotationDirection.Left, degrees.toInt / 90)
    case s"R$degrees" => Instruction.Rotate90Degrees(RotationDirection.Right, degrees.toInt / 90)
    case s"F$by" => Instruction.Forward(by.toInt)
  }

  private val input = Resources.lines("day12.txt").map(parseInstruction)

  println(solvePart1(input)) // 938
  println(solvePart2(input)) // 54404

}
