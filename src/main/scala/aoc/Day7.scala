package aoc

import aoc.util.Resources

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day7 extends App {

  private val TargetBag = "shiny gold bag"

  def solvePart1(graph: Map[String, List[(Int, String)]]): Int = {
    @tailrec
    def pathExists(queue: Queue[String], seen: Set[String]): Boolean = queue.dequeueOption match {
      case None => false
      case Some((head, tail)) =>
        if (head == TargetBag) true
        else {
          val toVisit = graph(head).map(_._2).filterNot(seen.contains)
          pathExists(tail ++ toVisit, seen ++ toVisit)
        }
    }

    graph.keys
      .filterNot(_ == TargetBag)
      .count(start => pathExists(Queue(start), Set(start)))
  }

  def solvePart2(graph: Map[String, List[(Int, String)]]): Int = {
    def totalCount(bag: String): Int =
      graph(bag)
        .map { case (count, next) =>
          count * totalCount(next)
        }
        .sum + 1

    totalCount(TargetBag) - 1 // we don't count target bag itself
  }

  private def parseGraph(input: List[String]): Map[String, List[(Int, String)]] = {
    def makeSingular(bagDescription: String) =
      if (bagDescription.contains("bags")) bagDescription.replace("bags", "bag")
      else bagDescription

    def parseLine(line: String): (String, List[(Int, String)]) = line match {
      case s"$bag contain no other bags" =>
        makeSingular(bag) -> Nil
      case s"$bag contain $bags" =>
        makeSingular(bag) -> bags.split(", ").toList.map {
          case s"$count $bag" => count.toInt -> makeSingular(bag)
        }
    }

    input.map(_.stripSuffix(".")).map(parseLine).toMap
  }

  private val input = parseGraph(Resources.lines("day7.txt"))

  println(solvePart1(input)) // 148
  println(solvePart2(input)) // 24867

}
