package aoc

import aoc.util.Resources

import scala.annotation.tailrec

object Day16 extends App {

  case class Input(fieldRules: Map[Field, FieldRule], yourTicket: Ticket, nearbyTickets: List[Ticket])

  case class Field(name: String) extends AnyVal

  case class FieldRule(validRanges: List[Range]) {
    def covers(value: Int): Boolean = validRanges.exists(_.contains(value))
  }

  case class Range(from: Int, to: Int) {
    def contains(value: Int): Boolean = from <= value && value <= to
  }

  case class Ticket(values: IndexedSeq[Int])

  def solvePart1(input: Input): Int = {
    def isValid(value: Int): Boolean =
      input.fieldRules.values.exists(_.covers(value))

    input.nearbyTickets
      .flatMap(_.values)
      .filterNot(isValid)
      .sum
  }

  def solvePart2(input: Input): Either[String, Long] = {
    def isValid(ticket: Ticket) =
      ticket.values.forall(v => input.fieldRules.values.exists(_.covers(v)))

    val validTickets = input.nearbyTickets.filter(isValid)
    val orderedFields = findFieldOrder(input.yourTicket :: validTickets, input.fieldRules)

    orderedFields
      .toRight("Could not find order of fields")
      .map { fields =>
        input.yourTicket.values.zip(fields)
          .filter(_._2.name.startsWith("departure"))
          .map(_._1.toLong)
          .product
      }
  }

  private def findFieldOrder(tickets: List[Ticket], rules: Map[Field, FieldRule]): Option[List[Field]] = {
    @tailrec
    def findOrder(possibleFieldsForIndices: Map[Int, Set[Field]], fieldIndices: Map[Field, Int]): Option[List[Field]] =
      if (possibleFieldsForIndices.isEmpty) Some(fieldIndices.toList.sortBy(_._2).map(_._1))
      else {
        val uniqueIndexField = possibleFieldsForIndices
          .collectFirst { case (i, fields) if fields.size == 1 => i -> fields.head }

        uniqueIndexField match {
          case None => None
          case Some((i, field)) =>
            val updMap = (possibleFieldsForIndices - i).view.mapValues(_ - field).toMap

            findOrder(updMap, fieldIndices + (field -> i))
        }
      }

    val possible = possibleFieldsForIndices(tickets, rules)

    findOrder(possible, Map())
  }

  private def possibleFieldsForIndices(tickets: List[Ticket], rules: Map[Field, FieldRule]): Map[Int, Set[Field]] = {
    def eliminatePossibilities(possibleFieldsForIndices: Map[Int, Set[Field]], ticket: Ticket): Map[Int, Set[Field]] =
      ticket.values.zipWithIndex
        .foldLeft(possibleFieldsForIndices) { case (acc, (value, i)) =>
          acc.updatedWith(i)(_.map(_.filter(rules(_).covers(value))))
        }

    val allFieldsPossibleForAllIndices = (0 until rules.size).map(_ -> rules.keySet).toMap
    tickets.foldLeft(allFieldsPossibleForAllIndices)(eliminatePossibilities)
  }

  private def parseInput(input: List[String]): Input = {
    def parseFieldWithRules(s: String) = s match {
      case s"$field: $from1-$to1 or $from2-$to2" =>
        Field(field) -> FieldRule(List(Range(from1.toInt, to1.toInt), Range(from2.toInt, to2.toInt)))
    }

    def parseTicket(s: String) = Ticket(s.split(",").map(_.toInt).toIndexedSeq)

    def parseFields(input: List[String]) = {
      val (fields, remaining) = input.span(_.nonEmpty)
      (fields.map(parseFieldWithRules).toMap, remaining.drop(1))
    }

    def parseYourTicket(input: List[String]) = input match {
      case "your ticket:" :: ticket :: "" :: remaining => (parseTicket(ticket), remaining)
    }

    def parseNearbyTickets(input: List[String]) = input match {
      case "nearby tickets:" :: tickets => tickets.map(parseTicket)
    }

    val (fieldRules, remaining0) = parseFields(input)
    val (yourTicket, remaining1) = parseYourTicket(remaining0)
    val nearbyTickets = parseNearbyTickets(remaining1)

    Input(fieldRules, yourTicket, nearbyTickets)
  }

  private val input = parseInput(Resources.lines("day16.txt"))

  println(solvePart1(input)) // 20060
  println(solvePart2(input)) // 2843534243843

}
