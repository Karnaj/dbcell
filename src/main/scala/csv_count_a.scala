package csv_count_a

import change._
import cell_parser._
import utils._

object CSVCountA {

  def propagateInB(p: Position, v: Int, bcs: List[BChange]): List[BChange] = {
    bcs match {
      case Nil => return Nil
      case bc :: q => {
        if(bc.b.isAfter(p)) return bcs
        else {
          if(bc.b.contains(p) && bc.counted == v) bc.v += 1
          return bc :: propagateInB(p, v, q)
        }
      }
    }
  }

  def propagateInA(p: Position, v: Int, acs: List[AChange]): List[AChange] = {
    acs match {
      case Nil => return Nil
      case ac :: q => {
        if(ac.p > p) return acs
        else {
          if(ac.p.x == p.x && ac.p.y == p.y) ac.old = v
          return propagateInA(p, v, q)
        }
      }
    }
  }

  private def countForCell(c: Change,
                           p: Position,
                           fbcs: List[BChange],
                           ubcs: List[BChange],
                           uacs: List[AChange]): (List[BChange], List[BChange], List[AChange]) = {
    c match {
      case _: BChange => return (fbcs, ubcs, uacs)
      case _: AChange => {
        val fbcrest = propagateInB(p, c.v, fbcs)
        val ubcrest = propagateInB(p, c.v, ubcs)
        val uacrest = propagateInA(p, c.v, uacs)
        return (fbcrest, ubcrest, uacrest)
      }
    }
  }

  private def countAInLine(cellsWithY: Iterator[(String, Int)],
                           x: Int,
                           fbcs: List[BChange],
                           ubcs: List[BChange],
                           uacs: List[AChange]): (List[BChange], List[BChange], List[AChange]) = {
    if(fbcs.isEmpty && ubcs.isEmpty && uacs.isEmpty)
      return (fbcs, ubcs, uacs)
    if(cellsWithY.isEmpty)
      return (fbcs, ubcs, uacs)
    val (cell, y) = cellsWithY.next
    val p: Position = new Position(x, y)
    val (fr, ur, ar) = countForCell(CellParser.parse(x, y, cell), p, fbcs, ubcs, uacs)
    countAInLine(cellsWithY, x, fr, ur, ar)
  }

  private def countAInSource(linesWithX: Iterator[(String, Int)],
                             fbcs: List[BChange],
                             ubcs: List[BChange],
                             uacs: List[AChange]): Unit = {

    if(fbcs.isEmpty && ubcs.isEmpty && uacs.isEmpty)
      return
    if(linesWithX.isEmpty)
      return
    val (str, x) = linesWithX.next
    val cellsWithY: Iterator[(String, Int)] = str.split(";").iterator.zipWithIndex
    val (fr, ur, ar): (List[BChange], List[BChange], List[AChange]) =
      countAInLine(cellsWithY, x, fbcs, ubcs, uacs)
    countAInSource(linesWithX, fr, ur, ar)
  }

  def countA(file: scala.io.BufferedSource, fbcs: List[BChange],
                      ubcs: List[BChange], uacs: List[AChange]): Unit = {
    val rest: List[AChange] = uacs.sortBy(c => (c.p.x, c.p.y))
    val frest: List[BChange] = Change.sortByBlockPosition(fbcs)
    val urest: List[BChange] = Change.sortByBlockPosition(ubcs)

    val linesWithX = file.getLines.zipWithIndex
    countAInSource(linesWithX, frest, urest, rest)
  }
}
