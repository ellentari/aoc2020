package aoc

import aoc.util.Resources

object Day1 extends App {

  def solvePart1(input: List[Int]): Int = {
    val set = input.toSet

    (for {
      x <- input
      y = 2020 - x if set.contains(y)
    } yield x * y)
      .headOption
      .getOrElse(throw new RuntimeException("no solution found"))
  }

  def solvePart2(input: List[Int]): Int = {
    val set = input.toSet

    (for {
      x <- input
      y <- input if y != x
      z = 2020 - (x + y) if set.contains(z)
    } yield x * y * z)
      .headOption
      .getOrElse(throw new RuntimeException("no solution found"))
  }

  private val input = Resources.lines("day1.txt").map(_.toInt)

  println(solvePart1(input)) // 494475
  println(solvePart2(input)) // 267520550

}
