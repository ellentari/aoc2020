package aoc

import scala.annotation.tailrec

object Day23 extends App {

  def solvePart1(cups: List[Int], moves: Int): String = {
    val initial = initVector(cups.iterator, cups.head, cups.last, cups.max)
    val result = playGame(moves, initial, cups.head, cups.min, cups.max)

    nextN(result, after = 1, n = cups.size - 1).mkString
  }

  def solvePart2(cups: List[Int], moves: Int): Long = {
    val maxCup = 1_000_000
    val initial = initVector(cups.iterator ++ (cups.max + 1 to maxCup).iterator, cups.head, maxCup, maxCup)
    val result = playGame(moves, initial, cups.head, cups.min, maxCup)

    nextN(result, after = 1, n = 2)
      .map(_.toLong)
      .product
  }

  private def initVector(iterator: Iterator[Int], first: Int, last: Int, maxElement: Int): Vector[Int] = {
    val array = new Array[Int](maxElement + 1)
    iterator.sliding(2).map(_.toList).foreach { case List(a, b) =>
      array(a) = b
    }
    array(last) = first // make it a circle
    array.toVector
  }

  private def playGame(targetMoves: Int, nextTo: Vector[Int], initial: Int, minCup: Int, maxCup: Int) = {
    @tailrec
    def findDestination(destination: Int, excluded: List[Int]): Int =
      if (destination < minCup) findDestination(maxCup, excluded)
      else if (excluded.contains(destination)) findDestination(destination - 1, excluded)
      else destination

    @tailrec
    def makeMove(move: Int, current: Int, next: Vector[Int]): Vector[Int] = {
      if (move > targetMoves) next
      else {
        val toRemove = nextN(next, current, n = 3)
        val nextAfterRemoved = next(toRemove.last)

        val destination = findDestination(current - 1, toRemove)
        val destinationNext = next(destination)

        val updNext = next
          .updated(current, nextAfterRemoved)
          .updated(destination, toRemove.head)
          .updated(toRemove.last, destinationNext)

        makeMove(move + 1, nextAfterRemoved, updNext)
      }
    }

    makeMove(move = 1, initial, nextTo)
  }

  private def nextN(next: Vector[Int], after: Int, n: Int): List[Int] =
    Iterator.iterate(next(after))(next).take(n).toList

  private val sample = "389125467".toIndexedSeq.map(_.asDigit).toList
  private val input = "327465189".toIndexedSeq.map(_.asDigit).toList

  println(solvePart1(sample, moves = 100)) // 67384529
  println(solvePart1(input, moves = 100)) // 82934675
  println(solvePart2(sample, moves = 10_000_000)) // 149245887792
  println(solvePart2(input, moves = 10_000_000)) // 474600314018

}
