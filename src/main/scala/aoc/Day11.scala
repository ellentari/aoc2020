package aoc

import aoc.util.Resources

import scala.annotation.tailrec

object Day11 extends App {

  type State = IndexedSeq[IndexedSeq[Cell]]

  sealed trait Cell extends Product with Serializable { self =>
    def isOccupied: Boolean = self match {
      case Cell.Occupied => true
      case _ => false
    }
  }
  object Cell {
    case object Empty extends Cell
    case object Seat extends Cell
    case object Occupied extends Cell
  }

  private val ds = List((-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1))

  def solvePart1(input: State): Int = {
    def adjacentCells(state: State, i: Int, j: Int) =
      ds.map { case (di, dj) => (i + di, j + dj) }
        .filter { case (i, j) => withinBounds(state, i, j) }
        .map { case (i, j) => state(i)(j) }

    val finalState = runSimulation(input, occupiedThreshold = 4, adjacentCells)

    finalState.flatten.count(_.isOccupied)
  }

  def solvePart2(input: State): Int = {
    def adjacentCells(state: State, i: Int, j: Int) = {
      def findFirstNonEmpty(di: Int, dj: Int) =
        Iterator.iterate((i + di, j + dj)) { case (i, j) => (i + di, j + dj) }
          .takeWhile { case (i, j) => withinBounds(state, i, j) }
          .map { case (i, j) => state(i)(j) }
          .find(_ != Cell.Empty)

      ds.flatMap((findFirstNonEmpty _).tupled)
    }

    val finalState = runSimulation(input, occupiedThreshold = 5, adjacentCells)

    finalState.flatten.count(_.isOccupied)
  }

  private def runSimulation(
    initial: State,
    occupiedThreshold: Int,
    adjacentCells: (State, Int, Int) => List[Cell]
  ): State = {

    def nextState(state: State) =
      state.zipWithIndex
        .map { case (row, i) =>
          row.zipWithIndex.map { case (cell, j) =>
            val occupiedCount = adjacentCells(state, i, j).count(_.isOccupied)

            cell match {
              case Cell.Seat if occupiedCount == 0                     => Cell.Occupied
              case Cell.Occupied if occupiedCount >= occupiedThreshold => Cell.Seat
              case _                                                   => cell
            }
          }
        }

    @tailrec
    def loop(state: State): State = {
      val next = nextState(state)

      if (diffCount(state, next) == 0) state
      else loop(next)
    }

    loop(initial)
  }

  private def diffCount(state1: State, state2: State) =
    state1.zip(state2)
      .map { case (row1, row2) =>
        row1.zip(row2).count { case (cell1, cell2) =>
          cell1 != cell2
        }
      }
      .sum

  private def withinBounds(state: State, i: Int, j: Int) =
    i >= 0 && i < state.length && j >= 0 && j < state(i).length

  private def parseRow(row: String): IndexedSeq[Cell] =
    row.map {
      case '.' => Cell.Empty
      case 'L' => Cell.Seat
      case '#' => Cell.Occupied
    }

  private val input = Resources.lines("day11.txt").map(parseRow).toIndexedSeq

  println(solvePart1(input)) // 2254
  println(solvePart2(input)) // 2004
}
