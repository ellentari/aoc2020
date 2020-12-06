package aoc

import aoc.util.Resources

object Day6 extends App {

  def solvePart1(input: List[List[String]]): Int =
    input.map(reduce(_ | _)).map(_.size).sum

  def solvePart2(input: List[List[String]]): Int =
    input.map(reduce(_ & _)).map(_.size).sum

  private def reduce(reduce: (Set[Char], Set[Char]) => Set[Char])(answers: List[String]) =
    answers.map(_.toSet).reduceOption(reduce).getOrElse(Set())

  private val input = Resources.string("day6.txt").split("\n\n")
    .map(_.split("\n").toList)
    .toList

  println(solvePart1(input)) // 6680
  println(solvePart2(input)) // 3117

}
