package aoc

import aoc.util.Resources

object Day2 extends App {

  case class Password(value: String) extends AnyVal

  /**
   * The password policy indicates the minimum [[min]] and maximum [[max]] number of times
   * a given [[character]] must appear for the password to be valid.
   */
  case class Policy1(min: Int, max: Int, character: Char)

  /**
   * Describes [[position1]] and [[position2]] in the password (positions are 1-based).
   * Exactly one of these position must contain the given [[character]].
   */
  case class Policy2(position1: Int, position2: Int, character: Char)

  def solvePart1(input: List[(Policy1, Password)]): Int =
    input.count { case (policy, password) =>
      val count = password.value.count(_ == policy.character)

      policy.min <= count && count <= policy.max
    }

  def solvePart2(input: List[(Policy2, Password)]): Int =
    input.count { case (policy, password) =>
      val firstMatched = password.value(policy.position1 - 1) == policy.character
      val secondMatched = password.value(policy.position2 - 1) == policy.character

      firstMatched ^ secondMatched
    }

  private def parsePolicyAndPassword[P](constructPolicy: (Int, Int, Char) => P): String => (P, Password) = {
    case s"$num1-$num2 $character: $password" =>
      (constructPolicy(num1.toInt, num2.toInt, character.charAt(0)), Password(password))
  }

  private val input = Resources.lines("day2.txt")

  println(solvePart1(input.map(parsePolicyAndPassword(Policy1)))) // 614
  println(solvePart2(input.map(parsePolicyAndPassword(Policy2)))) // 354

}
