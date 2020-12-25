package aoc

object Day25 extends App {

  private val Mod = 20201227

  def solvePart1(cardPublicKey: Int, doorPublicKey: Int): Int =
    transform(doorPublicKey, findLoopSize(cardPublicKey))

  private def findLoopSize(publicKey: Int): Int =
    Iterator.iterate(1)(_ + 1)
      .find(transform(subjectNumber = 7, _) == publicKey)
      .get

  private def transform(subjectNumber: Int, loopSize: Int) =
    BigInt(subjectNumber).modPow(loopSize, Mod).toInt

  println(solvePart1(cardPublicKey = 5764801, doorPublicKey = 17807724)) // 14897079
  println(solvePart1(cardPublicKey = 12090988, doorPublicKey = 240583)) // 3015200

}
