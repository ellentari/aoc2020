package aoc

import aoc.util.Resources

object Day3 extends App {

  case class Slope(right: Int, down: Int)

  case class Coordinate(y: Int = 0, x: Int = 0) {
    def next(slope: Slope): Coordinate =
      Coordinate(y + slope.down, x + slope.right)

    def xMod(v: Int): Coordinate =
      copy(x = x % v)
  }

  def solvePart1(input: IndexedSeq[String]): Int =
    countTrees(input)(Slope(3, 1))

  def solvePart2(input: IndexedSeq[String]): Long =
     List(
       Slope(1, 1),
       Slope(3, 1),
       Slope(5, 1),
       Slope(7, 1),
       Slope(1, 2)
      )
       .map(countTrees(input))
       .map(_.toLong)
       .product

  private def countTrees(input: IndexedSeq[String])(slope: Slope): Int =
    if (input.isEmpty) 0
    else {
      val length = input(0).length

      Iterator.iterate(Coordinate())(_.next(slope).xMod(length))
        .takeWhile(_.y < input.length)
        .map(c => input(c.y)(c.x))
        .count(_ == '#')
    }

  private val input = Resources.lines("day3.txt").toIndexedSeq

  println(solvePart1(input)) // 284
  println(solvePart2(input)) // 3510149120

}
