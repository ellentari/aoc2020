package aoc

import aoc.util.Resources

import scala.annotation.tailrec
import scala.math.Ordered._

object Day22 extends App {

  case class Deck(cards: List[Card]) {
    def +(card: Card): Deck = Deck(cards :+ card)
    def size: Int = cards.size
    def take(n: Int): Deck = Deck(cards.take(n))

    def draw: Option[(Card, Deck)] =
      cards match {
        case Nil => None
        case top :: remaining => Some(top, Deck(remaining))
      }

    def score: Int =
      cards.zip(cards.indices.reverse.map(_ + 1))
        .map { case (card, multiplier) => card.value * multiplier }
        .sum
  }

  case class Card(value: Int)
  object Card {
    implicit val cardOrdering: Ordering[Card] = Ordering.by(_.value)
  }

  sealed trait Outcome extends Product with Serializable
  object Outcome {
    case object Player1Wins extends Outcome
    case object Player2Wins extends Outcome
  }

  def solvePart1(player1: Deck, player2: Deck): Int = {
    @tailrec
    def play(player1: Deck, player2: Deck): (Outcome, Deck) =
      (player1.draw, player2.draw) match {
        case (None, _) => (Outcome.Player2Wins, player2)
        case (_, None) => (Outcome.Player1Wins, player1)
        case (Some((top1, deck1)), Some((top2, deck2))) =>
          chooseWinner(top1, top2) match {
            case Outcome.Player1Wins => play(deck1 + top1 + top2, deck2)
            case Outcome.Player2Wins => play(deck1, deck2 + top2 + top1)
          }
      }

    val (_, winningDeck) = play(player1, player2)

    winningDeck.score
  }

  private def chooseWinner(player1: Card, player2: Card): Outcome =
    if (player1 > player2) Outcome.Player1Wins
    else Outcome.Player2Wins

  def solvePart2(player1: Deck, player2: Deck) = {
    def play(player1: Deck, player2: Deck, seen: Set[(Deck, Deck)]): (Outcome, Deck) =
      if (seen.contains((player1, player2))) (Outcome.Player1Wins, player1)
      else (player1.draw, player2.draw) match {
        case (None, _) => (Outcome.Player2Wins, player2)
        case (_, None) => (Outcome.Player1Wins, player1)
        case (Some((top1, deck1)), Some((top2, deck2))) =>
          val newSeen = seen + (player1 -> player2)

          val outcome =
            if (deck1.size >= top1.value && deck2.size >= top2.value)
              play(deck1.take(top1.value), deck2.take(top2.value), newSeen)._1
            else chooseWinner(top1, top2)

          outcome match {
            case Outcome.Player1Wins => play(deck1 + top1 + top2, deck2, newSeen)
            case Outcome.Player2Wins => play(deck1, deck2 + top2 + top1, newSeen)
          }
      }

    val (_, winningDeck) = play(player1, player2, Set.empty)

    winningDeck.score
  }

  private def parseDeck(s: String): Deck = {
    def parseCard(s: String): Card = Card(s.toInt)

    Deck(s.split("\n").drop(1).map(parseCard).toList)
  }

  private val input = Resources.string("day22.txt").split("\n\n").toList
  private val player1Deck = parseDeck(input.head)
  private val player2Deck = parseDeck(input.tail.head)

  println(solvePart1(player1Deck, player2Deck)) // 32489
  println(solvePart2(player1Deck, player2Deck)) // 35676
}
