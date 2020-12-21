package aoc

import aoc.util.Resources

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.concurrent.duration.DurationLong

object Day20 extends App {

  case class Index(row: Int, column: Int) {
    def up: Option[Index] = Option.when(row > 0)(copy(row = row - 1))
    def right(width: Int): Option[Index] = Option.when(column < width - 1)(copy(column = column + 1))
    def left: Option[Index] = Option.when(column > 0)(copy(column = column - 1))
    def down(height: Int): Option[Index] = Option.when(row < height - 1)(copy(row = row + 1))
    def offset(o: Index): Index = Index(row = row + o.row, column = column + o.column)
  }

  case class Tile(content: IndexedSeq[String]) {

    val height: Int = content.length
    val width: Int = content.head.length

    def topBorder: String = content.head
    def rightBorder: String = content.map(_.last).mkString
    def bottomBorder: String = content.last
    def leftBorder: String = content.map(_.head).mkString

    def flip: Tile = Tile(content.map(_.reverse))

    def rotateClockwise: Tile = {
      val rotated = Array.ofDim[Char](width, height)

      for {
        i <- content.indices
        j <- content(i).indices
      } {
        rotated(j)(height - i - 1) = content(i)(j)
      }

      Tile(rotated.map(new String(_)).toVector)
    }

    def possibleModifications: List[Tile] =
      List(
        this,
        flip,
        rotateClockwise.flip,
        rotateClockwise.rotateClockwise.flip,
        rotateClockwise.rotateClockwise.rotateClockwise.flip,
        rotateClockwise,
        rotateClockwise.rotateClockwise,
        rotateClockwise.rotateClockwise.rotateClockwise,
      )

    def isTopAdjacentTo(top: Tile): Boolean = topBorder == top.bottomBorder
    def isBottomAdjacentTo(bottom: Tile): Boolean = bottom.isTopAdjacentTo(this)
    def isRightAdjacentTo(right: Tile): Boolean = rightBorder == right.leftBorder
    def isLeftAdjacentTo(left: Tile): Boolean = left.isRightAdjacentTo(this)

    def noBorders: Tile = Tile(content.tail.init.map(_.tail.init))

    def appendRight(other: Tile): Tile = Tile(content.zip(other.content).map(t => t._1 + t._2))
    def appendBottom(other: Tile): Tile = Tile(content ++ other.content)

    def matchPattern(pattern: Pattern): List[Index] = {
      def matchPattern(part: IndexedSeq[String], maxOffset: Int): List[Int] =
        (0 to maxOffset)
          .filter { offset =>
            pattern.atOffset(Index(row = 0, column = offset)).toMatch
              .forall { case (index, char) =>
                part(index.row)(index.column) == char
              }
          }
          .toList

      if (pattern.height == 0 || pattern.width == 0) Nil
      else {
        val maxOffset = width - pattern.width

        content.sliding(pattern.height).zipWithIndex
          .flatMap { case (part, row) =>
            matchPattern(part, maxOffset).map(Index(row, _))
          }
          .toList
      }
    }

    def replaceAll(startingLocations: List[Index], pattern: Pattern, replacement: Char): Tile = {
      val result = content.map(_.toCharArray).toArray

      for {
        offset <- startingLocations
        idx <- pattern.atOffset(offset).toMatch.keys
      } {
        result(idx.row)(idx.column) = replacement
      }

      Tile(result.toVector.map(new String(_)))
    }
  }

  case class Pattern(toMatch: Map[Index, Char]) {
    def atOffset(offset: Index): Pattern =
      Pattern(toMatch.map {case (k, v) => k.offset(offset) -> v })

    def width: Int =
      (for {
        min <- toMatch.keys.map(_.column).minOption
        max <- toMatch.keys.map(_.column).maxOption
      } yield max - min + 1).getOrElse(0)

    def height: Int =
      (for {
        min <- toMatch.keys.map(_.row).minOption
        max <- toMatch.keys.map(_.row).maxOption
      } yield max - min + 1).getOrElse(0)
  }

  object Pattern {
    def from(pattern: List[String], ignored: Char*): Pattern = {
      val toMatch = (for {
        (row, r) <- pattern.zipWithIndex
        (ch, c) <- row.zipWithIndex if !ignored.contains(ch)
      } yield Index(r, c) -> ch).toMap

      Pattern(toMatch)
    }
  }

  type TileId = Long

  case class IdentifiedTile(id: TileId, tile: Tile) {
    def possibleModifications: List[IdentifiedTile] =
      tile.possibleModifications.map(tile => copy(tile = tile))

    def isTopAdjacentTo(top: IdentifiedTile): Boolean = tile.isTopAdjacentTo(top.tile)
    def isBottomAdjacentTo(bottom: IdentifiedTile): Boolean = tile.isBottomAdjacentTo(bottom.tile)
    def isRightAdjacentTo(right: IdentifiedTile): Boolean = tile.isRightAdjacentTo(right.tile)
    def isLeftAdjacentTo(left: IdentifiedTile): Boolean = tile.isLeftAdjacentTo(left.tile)
  }

  case class AssembledTiles(underlying: Map[Index, IdentifiedTile]) {
    val height: Int = underlying.keys.map(_.row).max + 1
    val width: Int = underlying.keys.map(_.column).max + 1

    def ++(tiles: List[(Index, IdentifiedTile)]): AssembledTiles =
      AssembledTiles(underlying ++ tiles)

    def topLeft: IdentifiedTile = underlying(Index(0, 0))
    def topRight: IdentifiedTile = underlying(Index(0, width - 1))
    def bottomLeft: IdentifiedTile = underlying(Index(height - 1, 0))
    def bottomRight: IdentifiedTile = underlying(Index(height - 1, width - 1))

    def corners: List[IdentifiedTile] = List(topLeft, topRight, bottomRight, bottomLeft)

    def asGrid: IndexedSeq[IndexedSeq[IdentifiedTile]] =
      (0 until height)
        .map(row => (0 until width).map(col => underlying(Index(row, col))))

    def toTile: Tile =
      asGrid.view.map(_.view.map(_.tile.noBorders).reduceLeft(_.appendRight(_)))
        .reduceLeft(_.appendBottom(_))

    def tileIds: Set[TileId] = underlying.values.map(_.id).toSet

  }

  def solveBothParts(tiles: List[IdentifiedTile]) =
    assemble(tiles).map { solution =>
      println(s"Part 1: ${part1(solution)}")
      println(s"Part 2: ${part2(solution)}")
    }

  private def part1(assembled: AssembledTiles): Long =
    assembled.corners.map(_.id).product

  private def part2(assembled: AssembledTiles): Option[Int] =
    assembled.toTile
      .possibleModifications.view
      .map(tile => tile -> tile.matchPattern(TargetPattern))
      .find(_._2.nonEmpty)
      .map { case (tile, locations) =>
        tile.replaceAll(locations, TargetPattern, replacement = 'O')
      }
      .map(_.content.map(_.count(_ == '#')).sum)

  private def assemble(tiles: List[IdentifiedTile]): Option[AssembledTiles] =
    assemble(tiles, Math.sqrt(tiles.length).toInt)

  private def assemble(tiles: List[IdentifiedTile], targetSize: Int): Option[AssembledTiles] = {

    @tailrec
    def bfs(queue: Queue[Index], result: AssembledTiles): Option[AssembledTiles] =
      queue.dequeueOption match {
        case None => Option.when(result.tileIds == tiles.map(_.id).toSet)(result)
        case Some((currentIndex, remaining)) =>
          def findMatch(
            adjacentIndex: Index => Option[Index],
            isAdjacent: IdentifiedTile => Boolean,
            additionallyExclude: Option[(Index, IdentifiedTile)]*
          ): Option[(Index, IdentifiedTile)] = {
            val used = result.tileIds ++ additionallyExclude.flatten.map(_._2.id)

            adjacentIndex(currentIndex)
              .filterNot(result.underlying.contains)
              .flatMap { index =>
                tiles.view
                  .filterNot(tile => used.contains(tile.id))
                  .flatMap(_.possibleModifications)
                  .find(isAdjacent)
                  .map(index -> _)
              }
          }

          val currentTile = result.underlying(currentIndex)

          val top = findMatch(_.up, currentTile.isTopAdjacentTo)
          val bottom = findMatch(_.down(targetSize), currentTile.isBottomAdjacentTo, top)
          val left = findMatch(_.left, currentTile.isLeftAdjacentTo, top, bottom)
          val right = findMatch(_.right(targetSize), currentTile.isRightAdjacentTo, top, bottom, left)

          val next = List(top, bottom, left, right).flatten

          bfs(remaining ++ next.map(_._1), result ++ next)
      }

    tiles.view
      .flatMap(_.possibleModifications)
      .map(firstTile => bfs(Queue(Index(0, 0)), AssembledTiles(Map(Index(0, 0) -> firstTile))))
      .find(_.isDefined)
      .flatten
  }

  private def parseTiles(s: String): List[IdentifiedTile] = {
    def parseSingleTile(s: String): IdentifiedTile = {
      val lines = s.split("\n").toIndexedSeq
      val id = lines.head match {
        case s"Tile $id:" => id.toLong
      }
      IdentifiedTile(id, Tile(lines.tail))
    }

    s.split("\n\n").toList.map(parseSingleTile)
  }

  private val sample = parseTiles(Resources.string("day20-sample.txt"))
  private val input = parseTiles(Resources.string("day20-input.txt"))
  private val TargetPattern = Pattern.from(List(
    "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   "
  ), ignored = ' ')

  solveBothParts(sample) // Part 1: 20899048083289, Part 2: 273
  solveBothParts(input) // Part 1: 11788777383197, Part 2: 2242

}
