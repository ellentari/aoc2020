package aoc

import aoc.util.Resources

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day18 extends App {

  sealed trait Token
  object Token {
    sealed trait Infix extends Token
    sealed trait Postfix extends Token

    sealed trait HasPrecedence extends Infix

    sealed trait Operator extends HasPrecedence with Postfix
    object Operator {
      case object Sum extends Operator
      case object Multiplication extends Operator
    }

    case class Operand(value: Long) extends Infix with Postfix {
      def +(op: Operand): Operand = Operand(value + op.value)
      def *(op: Operand): Operand = Operand(value * op.value)
    }

    sealed trait Bracket extends Infix
    object Bracket {
      case object Open extends Bracket with HasPrecedence
      case object Closing extends Bracket
    }
  }

  def solvePart1(input: List[List[Token.Infix]]) = solve(input, _ => 0)

  def solvePart2(input: List[List[Token.Infix]]) =
    solve(input, {
      case Token.Operator.Sum => 1
      case Token.Operator.Multiplication => 0
    })

  private def solve(input: List[List[Token.Infix]], precedence: Token.Operator => Int): Long =
    input.map(convertToPostfix(precedence)).map(evalPostfix).sum

  private def convertToPostfix(precedence: Token.Operator => Int)(tokens: List[Token.Infix]): List[Token.Postfix] = {
    @tailrec
    def unwind(
      stack: List[Token.HasPrecedence],
      queue: Queue[Token.Postfix]
    ): (List[Token.HasPrecedence], Queue[Token.Postfix]) =
      stack match {
        case Nil => (Nil, queue)
        case Token.Bracket.Open :: tail => (tail, queue)
        case (o: Token.Operator) :: tail =>
          unwind(tail, queue.enqueue(o))
      }

    @tailrec
    def pushOp(
      op: Token.Operator,
      stack: List[Token.HasPrecedence],
      queue: Queue[Token.Postfix]
    ): (List[Token.HasPrecedence], Queue[Token.Postfix]) =
      stack match {
        case (head: Token.Operator) :: tail if precedence(head) >= precedence(op) =>
          pushOp(op, tail, queue.enqueue(head))
        case _ => (op :: stack, queue)
      }

    def processToken(token: Token.Infix, stack: List[Token.HasPrecedence], queue: Queue[Token.Postfix]) =
      token match {
        case o: Token.Operator => pushOp(o, stack, queue)
        case o: Token.Operand => (stack, queue.enqueue(o))
        case Token.Bracket.Open => (Token.Bracket.Open :: stack, queue)
        case Token.Bracket.Closing => unwind(stack, queue)
      }

    val (stack, queue) = tokens.foldLeft((List.empty[Token.HasPrecedence], Queue.empty[Token.Postfix])) {
      case ((s, q), token) => processToken(token, s, q)
    }

    unwind(stack, queue)._2.toList
  }

  private def evalPostfix(tokens: List[Token.Postfix]): Long = {
    def eval(op: Token.Operator, left: Token.Operand, right: Token.Operand) = op match {
      case Token.Operator.Sum => left + right
      case Token.Operator.Multiplication => left * right
    }

    def processToken(stack: List[Token.Operand], token: Token.Postfix) = token match {
      case o: Token.Operand => o :: stack
      case o: Token.Operator => stack match {
        case right :: left :: rest => eval(o, left, right) :: rest
        case _ => throw new RuntimeException(s"unexpected stack contents: $stack")
      }
    }

    tokens.foldLeft(List.empty[Token.Operand])(processToken)
      .head.value
  }

  private def parseInfix(input: String): List[Token.Infix] =
    input
      .replaceAll("([()])", " $1 ")
      .trim
      .split("\\s+")
      .map {
        case "+" => Token.Operator.Sum
        case "*" => Token.Operator.Multiplication
        case "(" => Token.Bracket.Open
        case ")" => Token.Bracket.Closing
        case literal => Token.Operand(literal.toLong)
      }
      .toList

  private val input = Resources.lines("day18.txt").map(parseInfix)

  println(solvePart1(input)) // 53660285675207
  println(solvePart2(input)) // 141993988282687
}
