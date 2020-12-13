package aoc

import aoc.util.Resources

import scala.annotation.tailrec
import scala.util.{Success, Try}

object Day13 extends App {

  def solvePart1(earliestTimestamp: Int, buses: List[Int]) = {
    val (bus, earliestDeparture) = buses
      .map { bus => (bus, (earliestTimestamp / bus + 1) * bus) }
      .minBy(_._2)

    val timeToWait = earliestDeparture - earliestTimestamp

    bus * timeToWait
  }

  def solvePart2(input: List[String]) = {
    val (buses, mods) = input.zipWithIndex
      .filterNot(_._1 == "x")
      .map { case (str, i) => (str.toInt, i) }
      .map { case (bus, i) => (bus, math.floorMod(-i, bus)) }
      .unzip

    chineseRemainder(buses.map(_.toLong), mods.map(_.toLong))
  }

  /**
   * https://rosettacode.org/wiki/Chinese_remainder_theorem
   */
  private def chineseRemainder(n: List[Long], a: List[Long]): Option[Long] = {
    require(n.size == a.size)
    val prod = n.product

    @tailrec
    def iter(n: List[Long], a: List[Long], sm: Long): Long = {
      def mulInv(a: Long, b: Long): Long = {
        @tailrec
        def loop(a: Long, b: Long, x0: Long, x1: Long): Long = {
          if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0) else x1
        }

        if (b == 1) 1
        else {
          val x1 = loop(a, b, 0, 1)
          if (x1 < 0) x1 + b else x1
        }
      }

      if (n.nonEmpty) {
        val p = prod / n.head

        iter(n.tail, a.tail, sm + a.head * mulInv(p, n.head) * p)
      } else sm
    }

    Try {
      iter(n, a, 0) % prod
    } match {
      case Success(v) => Some(v)
      case _          => None
    }
  }

  private val input = Resources.lines("day13.txt")
  private val busesInput = input.tail.head.split(",").toList

  println(solvePart1(input.head.toInt, busesInput.filterNot(_ == "x").map(_.toInt))) // 259
  println(solvePart2(busesInput)) // 210612924879242

}