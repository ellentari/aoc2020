package aoc

import aoc.util.Resources

object Day19 extends App {

  sealed trait Rule

  object Rule {
    case class PrefixMatch(prefix: String) extends Rule
    case class Sequence(ruleNumbers: List[Int]) extends Rule
    case class Any(rules: List[Rule]) extends Rule
  }

  def solvePart1(rules: Map[Int, Rule], messages: List[String]) =
    messages.count(matchesRules(_, rules))

  def solvePart2(rules: Map[Int, Rule], messages: List[String]) = {
    val updatedRules = rules
      .updated(8, Rule.Any(List(Rule.Sequence(List(42)), Rule.Sequence(List(42, 8)))))
      .updated(11, Rule.Any(List(Rule.Sequence(List(42, 31)), Rule.Sequence(List(42, 11, 31)))))

    solvePart1(updatedRules, messages)
  }

  private def matchesRules(message: String, rules: Map[Int, Rule]): Boolean = {
    def loop(message: String, toMatch: List[Rule]): Boolean =
      if (message.isEmpty) toMatch.isEmpty
      else toMatch match {
        case Nil => false
        case head :: tail =>
          head match {
            case Rule.PrefixMatch(prefix) =>
              if (message.startsWith(prefix)) loop(message.substring(prefix.length), tail)
              else false
            case Rule.Sequence(numbers) => loop(message, numbers.map(rules) ++ tail)
            case Rule.Any(rules) => rules.map(_ :: tail).exists(loop(message, _))
          }
      }

    loop(message, List(rules(0)))
  }

  private def parseRuleLine(input: String): (Int, Rule) = {
    def parseRule(rule: String): Rule = {
      if (rule.startsWith("\"") && rule.endsWith("\""))
        Rule.PrefixMatch(rule.substring(1, rule.length - 1))
      else {
        val any = rule.split(" \\| ")
          .map(ss => Rule.Sequence(ss.split(" ").map(_.toInt).toList))
          .toList

        any match {
          case single :: Nil => single
          case _ => Rule.Any(any)
        }
      }
    }

    input match {
      case s"$num: $rule" => num.toInt -> parseRule(rule)
    }
  }

  private val input = Resources.lines("day19.txt")
  private val inputRules = input.takeWhile(_.nonEmpty).map(parseRuleLine).toMap
  private val inputMessages = input.dropWhile(_.nonEmpty).drop(1)

  println(solvePart1(inputRules, inputMessages)) // 113
  println(solvePart2(inputRules, inputMessages)) // 253

}
