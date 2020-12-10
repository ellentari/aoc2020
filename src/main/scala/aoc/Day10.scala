package aoc

import aoc.util.Resources

import scala.annotation.tailrec

object Day10 extends App {

  case class Output(joltage: Int) extends AnyVal {
    def diff(other: Output): Int =
      joltage - other.joltage
  }
  object Output {
    val Zero: Output = Output(0)
  }

  private val MaxDiff = 3

  def solvePart1(sortedOutputs: List[Output]): Option[Int] = {
    @tailrec
    def loop(outputs: List[Output], previous: Output, diffCounts: Map[Int, Int]): Option[Map[Int, Int]] =
      outputs match {
        case Nil => Some(diffCounts)
        case output :: remaining =>
          val diff = output.diff(previous)
          if (diff <= MaxDiff)
            loop(remaining, output, diffCounts + (diff -> (diffCounts.getOrElse(diff, 0) + 1)))
          else None
      }

    loop(sortedOutputs, Output.Zero, Map()).map { diffCounts =>
      diffCounts.getOrElse(1, 0) * diffCounts.getOrElse(3, 0)
    }
  }

  def solvePart2(sortedOutputs: List[Output]): Long = {
    val dp = Array.ofDim[Long](sortedOutputs.length)

    dp(0) = 1

    for {
      i <- 1 until sortedOutputs.length
      output = sortedOutputs(i)
    } {
      dp(i) = (1 to MaxDiff)
        .map(i - _)
        .filter(j => j >= 0 && output.diff(sortedOutputs(j)) <= MaxDiff)
        .map(dp)
        .sum
    }

    dp.last
  }

  private val input = Resources.lines("day10.txt").map(_.toInt).map(Output(_)).sortBy(_.joltage)
  private val builtIn = Output(input.last.joltage + MaxDiff)

  println(solvePart1(input :+ builtIn)) // 1890
  println(solvePart2(Output.Zero :: (input :+ builtIn))) // 49607173328384
}
