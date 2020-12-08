package aoc

import aoc.util.Resources

import scala.annotation.tailrec

object Day8 extends App {

  sealed trait TerminationResult extends Product with Serializable {
    def acc: Int
  }
  object TerminationResult {
    case class InfiniteLoopBroken(acc: Int) extends TerminationResult
    case class Normal(acc: Int) extends TerminationResult
  }

  sealed trait Instruction extends Product with Serializable
  object Instruction {
    case class Acc(by: Int) extends Instruction
    case class Jump(by: Int) extends Instruction
    case class NoOp(value: Int) extends Instruction
  }

  def solvePart1(input: IndexedSeq[Instruction]): Int = {
    runProgram(input).acc
  }

  def solvePart2(input: IndexedSeq[Instruction]): Option[Int] =
    (for {
      i <- input.indices.view
      updated <- input(i) match {
        case Instruction.NoOp(by) => Some(Instruction.Jump(by))
        case Instruction.Jump(by) => Some(Instruction.NoOp(by))
        case _                    => None
      }
      modifiedInput = input.updated(i, updated)
      result <- runProgram(modifiedInput) match {
        case TerminationResult.Normal(acc) => Some(acc)
        case _                             => None
      }
    } yield result).headOption

  private def runProgram(input: IndexedSeq[Instruction]): TerminationResult = {
    @tailrec
    def loop(pointer: Int, acc: Int, executed: Set[Int]): TerminationResult = {
      if (executed.contains(pointer)) TerminationResult.InfiniteLoopBroken(acc)
      else if (pointer < 0 || pointer >= input.length) TerminationResult.Normal(acc)
      else {
        val instruction = input(pointer)
        instruction match {
          case Instruction.NoOp(_)  => loop(pointer + 1, acc, executed + pointer)
          case Instruction.Acc(by)  => loop(pointer + 1, acc + by, executed + pointer)
          case Instruction.Jump(by) => loop(pointer + by, acc, executed + pointer)
        }
      }
    }

    loop(0, 0, Set())
  }

  private def parseInstruction(s: String) = s match {
    case s"nop $v" => Instruction.NoOp(v.toInt)
    case s"acc $v" => Instruction.Acc(v.toInt)
    case s"jmp $v" => Instruction.Jump(v.toInt)
  }

  private val input = Resources.lines("day8.txt").map(parseInstruction).toIndexedSeq

  println(solvePart1(input)) // 1446
  println(solvePart2(input)) // 1403
}
