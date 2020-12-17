package aoc

import aoc.util.Resources

object Day17 extends App {

  type Grid[C] = Map[C, Int]

  case class Coordinate2D(x: Int, y: Int) {
    def to3D: Coordinate3D = Coordinate3D(x, y, z = 0)
    def to4D: Coordinate4D = Coordinate4D(x, y, z = 0, w = 0)
  }

  case class Coordinate3D(x: Int, y: Int, z: Int) {
    def adjacent: List[Coordinate3D] =
      for {
        xi <- List(-1, 0, 1)
        yi <- List(-1, 0, 1)
        zi <- List(-1, 0, 1) if (xi, yi, zi) != (0, 0, 0)
      } yield Coordinate3D(x + xi, y + yi, y + zi)
  }

  case class Coordinate4D(x: Int, y: Int, z: Int, w: Int) {
    def adjacent: List[Coordinate4D] =
      for {
        xi <- List(-1, 0, 1)
        yi <- List(-1, 0, 1)
        zi <- List(-1, 0, 1)
        wi <- List(-1, 0, 1) if (xi, yi, zi, wi) != (0, 0, 0, 0)
      } yield Coordinate4D(x + xi, y + yi, z + zi, w + wi)
  }

  def solvePart1(grid: Grid[Coordinate3D]) = solve(grid)(_.adjacent)

  def solvePart2(grid: Grid[Coordinate4D]) = solve(grid)(_.adjacent)

  private def solve[C](grid: Grid[C])(getNeighbours: C => List[C]): Int = {
    def loop(grid: Grid[C]): Grid[C] = {
      val toCheck = grid.keys.flatMap(getNeighbours).toSet

      toCheck.foldLeft(grid) { case (acc, coordinate) =>
        val value = grid.getOrElse(coordinate, 0)
        val activeNeighbors = getNeighbours(coordinate).map(grid.getOrElse(_, 0)).sum

        val newValue =
          if (value == 1)
            if (activeNeighbors == 2 || activeNeighbors == 3) 1
            else 0
          else if (activeNeighbors == 3) 1
          else 0
          
        acc.updated(coordinate, newValue)
      }
    }

    val result = (1 to 6).foldLeft(grid)((acc, _) => loop(acc))
    result.values.sum
  }

  private def parseGrid(input: List[String]): Grid[Coordinate2D] =
    input.zipWithIndex.flatMap { case (row, x) =>
      row.zipWithIndex.map { case (cell, y) =>
        val value = cell match {
          case '#' => 1
          case '.' => 0
        }
        Coordinate2D(x, y) -> value
      }
    }.toMap

  private def map[C, CC](grid: Grid[C])(f: C => CC): Grid[CC] =
    grid.map { case (k, v) => f(k) -> v}

  private val input = parseGrid(Resources.lines("day17.txt"))
  
  println(solvePart1(map(input)(_.to3D))) // 295
  println(solvePart2(map(input)(_.to4D))) // 1972

}
