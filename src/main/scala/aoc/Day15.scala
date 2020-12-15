package aoc

import scala.annotation.tailrec

object Day15 extends App {

  case class LastTwo[A](last: A, beforeLast: Option[A] = None)

  def solvePart1(input: List[Int]): Int =
    solve(input, targetTurn = 2020)

  def solvePart2(input: List[Int]): Int =
    solve(input, targetTurn = 30_000_000)

  private def solve(startingNumbers: List[Int], targetTurn: Int) = {
    @tailrec
    def loop(turn: Int, lastNumber: Int, numberOccurrences: Map[Int, LastTwo[Int]]): Int = {
      val lastTwo = numberOccurrences(lastNumber)

      val number = lastTwo.beforeLast.fold(0)(lastTwo.last - _)

      if (turn == targetTurn) number
      else loop(turn + 1, number,
        numberOccurrences.updatedWith(number)(lastTwo => Some(LastTwo(turn, lastTwo.map(_.last)))))
    }

    val occurrences = startingNumbers.zip(startingNumbers.indices.map(_ + 1).map(LastTwo(_))).toMap

    loop(startingNumbers.length + 1, startingNumbers.last, occurrences)
  }

  private val input = "9,6,0,10,18,2,1".split(",").map(_.toInt).toList

  println(solvePart1(input)) // 1238
  println(solvePart2(input)) // 3745954

}
