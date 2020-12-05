package aoc

import aoc.util.Resources

import scala.annotation.tailrec

object Day5 extends App {

  private val RowsNumber = 128
  private val ColumnsNumber = 8
  private val SplitAt = 7

  sealed trait Instruction extends Product with Serializable
  case object Low extends Instruction
  case object High extends Instruction

  def solvePart1(input: List[String]): Int =
    input.map(getSeatID).max

  def solvePart2(input: List[String]): Option[Int] =
    input.map(getSeatID).sorted
      .sliding(3)
      .collectFirst {
        case List(first, second, third) if first + 1 == second && second + 1 != third =>
          second + 1
      }

  private def getSeatID(input: String): Int = {
    def instructions(from: Int, until: Int, f: PartialFunction[Char, Instruction]) =
      (from until until).map(input).map(f).toList

    val row = binPartition(instructions(0, SplitAt, {
      case 'F' => Low
      case 'B' => High
    }), 0, RowsNumber - 1)

    val column = binPartition(instructions(SplitAt, input.length, {
      case 'L' => Low
      case 'R' => High
    }), 0, ColumnsNumber - 1)

    row * 8 + column
  }

  @tailrec
  private def binPartition(instructions: List[Instruction], low: Int, high: Int): Int =
    instructions match {
      case Nil => low
      case head :: tail =>
        val mid = (low + high) / 2
        head match {
          case Low  => binPartition(tail, low, mid)
          case High => binPartition(tail, mid + 1, high)
        }
    }

  private val input = Resources.lines("day5.txt")

  println(solvePart1(input)) // 951
  println(solvePart2(input)) // 653

}
