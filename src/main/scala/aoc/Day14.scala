package aoc

import aoc.util.Resources

import java.lang.{Long => JLong}

import scala.annotation.tailrec

object Day14 extends App {

  type Address = Long
  type Value = Long

  sealed trait Instruction extends Product with Serializable
  object Instruction {
    case class Mask(mask: String) extends Instruction
    case class Mem(address: Address, value: Value) extends Instruction
  }

  case class State(mask: Option[BitMask] = None, memory: Map[Address, Value] = Map()) {

    def set(address: Address, value: Value): State =
      copy(memory = memory + (address -> value))

    def setAll(addresses: List[Address], value: Value): State =
      copy(memory = memory ++ addresses.map(_ -> value))
  }

  case class BitMask(value: String) {
    val ones: Long = JLong.parseLong(value.replaceAll("X", "0"), 2)
    val zeroes: Long = JLong.parseLong(value.replaceAll("X", "1"), 2)
    val floatingBits: List[Int] = value.zipWithIndex.filter(_._1 == 'X').map(_._2).map(value.length - 1 - _).toList
  }

  def solvePart1(instructions: List[Instruction]): Either[String, Value] = {
    def applyMem(instruction: Instruction.Mem, state: State) =
      state.mask.toRight("no mask")
        .map { mask =>
          state.set(instruction.address, (instruction.value | mask.ones) & mask.zeroes)
        }

    runInstructions(applyMem, instructions)
      .map(_.memory.values.sum)
  }

  def solvePart2(instructions: List[Instruction]): Either[String, Value] = {
    def applyMem(instruction: Instruction.Mem, state: State) =
      state.mask.toRight("no mask")
        .map { mask =>
          state.setAll(generateMemoryAddresses(instruction.address, mask), instruction.value)
        }

    runInstructions(applyMem, instructions)
      .map(_.memory.values.sum)
  }

  private def generateMemoryAddresses(address: Address, mask: BitMask): List[Address] = {
    @tailrec
    def loop(replaceIndexes: List[Int], acc: List[Address]): List[Address] =
      replaceIndexes match {
        case Nil => acc
        case i :: tail =>
          val one = 1L << i
          val zero = ~(1L << i)

          loop(tail, acc.flatMap(addr => List(addr | one, addr & zero)))
      }

    loop(mask.floatingBits, List(address | mask.ones))
  }

  private def runInstructions(
    applyMem: (Instruction.Mem, State) => Either[String, State],
    instructions: List[Instruction]
  ): Either[String, State] = {
    def applyInstruction(instruction: Instruction, state: State) = instruction match {
      case Instruction.Mask(mask) => Right(state.copy(mask = Some(BitMask(mask))))
      case m@Instruction.Mem(_, _) => applyMem(m, state)
    }

    @tailrec
    def loop(instructions: List[Instruction], state: State): Either[String, State] = instructions match {
      case Nil => Right(state)
      case instruction :: remaining =>
        applyInstruction(instruction, state) match {
          case Left(error) => Left(error)
          case Right(newState) => loop(remaining, newState)
        }
    }

    loop(instructions, State())
  }

  private def parseInstruction(s: String): Instruction = s match {
    case s"mask = $mask" => Instruction.Mask(mask)
    case s"mem[$addr] = $value" => Instruction.Mem(addr.toLong, value.toLong)
  }

  private val input = Resources.lines("day14.txt").map(parseInstruction)

  println(solvePart1(input)) // 13556564111697
  println(solvePart2(input)) // 4173715962894
}
