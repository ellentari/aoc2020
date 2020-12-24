package aoc

import aoc.util.Resources

import scala.annotation.tailrec

object Day24 extends App {

  sealed trait Instruction
  object Instruction {
    case object East extends Instruction
    case object SouthEast extends Instruction
    case object SouthWest extends Instruction
    case object West extends Instruction
    case object NorthWest extends Instruction
    case object NorthEast extends Instruction

    val All: List[Instruction] = List(
      Instruction.East,
      Instruction.SouthEast,
      Instruction.SouthWest,
      Instruction.West,
      Instruction.NorthWest,
      Instruction.NorthEast
    )
  }

  case class Coordinate(x: Int, y: Int) {
    def adjacent: List[Coordinate] = Instruction.All.map(applyInstruction)

    def applyInstruction(instruction: Instruction): Coordinate = instruction match {
      case Instruction.East => copy(x = x + 2)
      case Instruction.SouthEast => copy(x = x + 1, y = y + 1)
      case Instruction.SouthWest => copy(x = x - 1, y = y + 1)
      case Instruction.West => copy(x = x - 2)
      case Instruction.NorthWest => copy(x = x - 1, y = y - 1)
      case Instruction.NorthEast => copy(x = x + 1, y = y - 1)
    }
  }

  case class TileLocation(instructions: List[Instruction])

  sealed trait Color { self =>
    def flipped: Color = self match {
      case Color.Black => Color.White
      case Color.White => Color.Black
    }
  }
  object Color {
    case object Black extends Color
    case object White extends Color
  }

  def solvePart1(locations: List[TileLocation]) =
    buildTileMap(locations).count(_._2 == Color.Black)

  def solvePart2(locations: List[TileLocation]) =
    evolve(buildTileMap(locations)).count(_._2 == Color.Black)

  private def buildTileMap(locations: List[TileLocation]): Map[Coordinate, Color] =
    locations
      .map(_.instructions.foldLeft(Coordinate(x = 0, y = 0))(_.applyInstruction(_)))
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .map { case (coordinate, timesFlipped) =>
        coordinate -> (if (timesFlipped % 2 == 0) Color.White else Color.Black)
      }

  private def evolve(map: Map[Coordinate, Color]) = {

    def shouldColorChange(color: Color, adjacentBlackCount: Int) =
      (color, adjacentBlackCount) match {
        case (Color.Black, n) if n > 2 || n == 0 => true
        case (Color.White, 2) => true
        case _ => false
      }

    @tailrec
    def loop(times: Int, map: Map[Coordinate, Color]): Map[Coordinate, Color] = {
      if (times == 0) map
      else {
        val next = map.keySet.flatMap(_.adjacent)
          .map { coordinate =>
            val color = map.getOrElse(coordinate, Color.White)
            val count = coordinate.adjacent.map(map.getOrElse(_, Color.White)).count(_ == Color.Black)

            if (shouldColorChange(color, count)) (coordinate, color.flipped)
            else (coordinate, color)
          }
          .toMap

        loop(times - 1, next)
      }
    }

    loop(times = 100, map)
  }

  private val instructionMapping = Map(
    "e" -> Instruction.East,
    "se" -> Instruction.SouthEast,
    "sw" -> Instruction.SouthWest,
    "w" -> Instruction.West,
    "nw" -> Instruction.NorthWest,
    "ne" -> Instruction.NorthEast
  )

  private def parseTileLocation(s: String): TileLocation = {
    @tailrec
    def parseInstructions(s: String, acc: List[Instruction]): List[Instruction] =
      if (s.isEmpty) acc.reverse
      else {
        val (matchedPart, instruction) = instructionMapping
          .find { case (e, _) => s.startsWith(e) }
          .getOrElse(throw new RuntimeException("Unexpected instruction: " + s))

        parseInstructions(s.substring(matchedPart.length), instruction :: acc)
      }

    TileLocation(parseInstructions(s, Nil))
  }

  private val input = Resources.lines("day24.txt").map(parseTileLocation)

  println(solvePart1(input)) // 436
  println(solvePart2(input)) // 4133

}
