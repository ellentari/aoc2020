package aoc

import scala.annotation.tailrec

object Day23 extends App {

  type Cup = Int

  case class Circle[A](next: Map[A, A] = Map.empty[A, A], previous: Map[A, A] = Map.empty[A, A]) {
    def remove(value: A): Circle[A] = {
      val prevVal = previous(value)
      val nextVal = next(value)
      Circle((next - value).updated(prevVal, nextVal), (previous - value).updated(nextVal, prevVal))
    }

    def add(first: A, second: A): Circle[A] =
      Circle(next.updated(first, second), previous.updated(second, first))

    def insert(previousValue: A, value: A, nextValue: A): Circle[A] =
      Circle(
        next.updated(previousValue, value).updated(value, nextValue),
        previous.updated(value, previousValue).updated(nextValue, value)
      )

    def nextAfter(after: A, count: Int): List[A] = {
      @tailrec
      def loop(i: Int, current: A, acc: List[A]): List[A] =
        if (i == count) acc.reverse
        else {
          val nextValue = next(current)
          loop(i + 1, nextValue, nextValue :: acc)
        }

      loop(0, after, Nil)
    }
  }

  object Circle {
    def from[A](iterator: Iterator[A], first: A, last: A): Circle[A] =
      iterator.sliding(2).map(_.toList).foldLeft(Circle[A]()) {
        case (circle, List(a, b)) => circle.add(a, b)
      }
        .add(last, first)

    def from[A](iterable: Iterable[A]): Circle[A] =
      from(iterable.iterator, iterable.head, iterable.last)
  }

  def solvePart1(cups: List[Cup], moves: Int): String =
    playGame(moves, Circle.from(cups), cups.head, cups.max)
      .nextAfter(after = 1, count = 8)
      .mkString("")

  def solvePart2(cups: List[Cup], moves: Int): Long = {
    val maxCup = 1_000_000
    val huge = cups.iterator ++ (cups.max + 1 to maxCup)
    val circle = Circle.from(huge, cups.head, maxCup)

    playGame(moves, circle, cups.head, maxCup)
      .nextAfter(after = 1, count = 2)
      .map(_.toLong)
      .product
  }

  private def playGame(targetMoves: Int, circle: Circle[Cup], initial: Cup, max: Cup) = {
    @tailrec
    def suitableDestination(value: Cup, excluded: Set[Cup]): Cup =
      if (value < 1) suitableDestination(max, excluded)
      else if (excluded.contains(value)) suitableDestination(value - 1, excluded)
      else value

    @tailrec
    def makeMove(move: Int, current: Cup, circle: Circle[Cup]): Circle[Cup] = {
      if (move > targetMoves) circle
      else {
        val next = circle.next(current)
        val nextNext = circle.next(next)
        val nextNextNext = circle.next(nextNext)

        val destination = suitableDestination(current - 1, Set(next, nextNext, nextNextNext))
        val destinationNext = circle.next(destination)

        val updCircle = circle
          .remove(next)
          .remove(nextNext)
          .remove(nextNextNext)
          .insert(destination, next, destinationNext)
          .insert(next, nextNext, destinationNext)
          .insert(nextNext, nextNextNext, destinationNext)

        makeMove(move + 1, updCircle.next(current), updCircle)
      }
    }

    makeMove(move = 1, initial, circle)
  }

  private val sample = "389125467".toIndexedSeq.map(_.asDigit).toList
  private val input = "327465189".toIndexedSeq.map(_.asDigit).toList

  println(solvePart1(sample, moves = 100)) // 67384529
  println(solvePart1(input, moves = 100)) // 82934675
  println(solvePart2(sample, moves = 10_000_000)) // 149245887792
  println(solvePart2(input, moves = 10_000_000)) // 474600314018
}
