package aoc

import aoc.util.Resources

import scala.annotation.tailrec

object Day9 extends App {

  def solvePart1(input: List[Long], n: Int): Option[Long] = {
    def isSumOfTwo(number: Long, set: Set[Long]) =
      set.exists(a => set.contains(number - a))

    @tailrec
    def loop(afterPreamble: List[Long], all: List[Long], previousN: Set[Long]): Option[Long] =
      afterPreamble match {
        case Nil => None
        case head :: _ if !isSumOfTwo(head, previousN) => Some(head)
        case head :: tail => loop(tail, all.tail, previousN + head - all.head)
      }

    loop(input.drop(n), input, input.take(n).toSet)
  }

  def solvePart2(input: List[Long], targetSum: Long) = {
    @tailrec
    def loop(input: List[Long], sum: Long, answer: List[Long]): List[Long] =
      if (sum == targetSum) answer
      else if (sum > targetSum) loop(input, sum - answer.head, answer.tail)
      else input match {
        case Nil => Nil
        case head :: tail => loop(tail, sum + head, answer :+ head)
      }

    val result = loop(input, 0, Nil).sorted

    result.head + result.last
  }

  private val input = Resources.lines("day9.txt").map(_.toLong)

  private val part1Res = solvePart1(input, 25)
  println(part1Res) // 530627549
  part1Res.map(solvePart2(input, _)).foreach(println) // 77730285

}
