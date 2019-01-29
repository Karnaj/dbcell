package csv_count_a

import change._
import cell_parser._
import utils._

object CSVCountA {

  def propagateCellValue(p: Position, v: Int, bcs: List[BChange]) = {
    bcs.foreach { bc => if(bc.b.contains(p) && bc.counted == v) bc.v += 1 }
  }

  def processLine(str: String, x: Int, fbcs: List[BChange], ubcs: List[BChange],
                  uar: List[AChange]) = {
    var rest: List[AChange] = uar
    str.split(";").zipWithIndex.foreach { case (cell, y) =>
      while(!rest.isEmpty && rest.head.p.x <= x && rest.head.p.y < y)
        rest = rest.tail

      val p: Position = new Position(x, y)
      CellParser.parse(x, y, cell) match {
        case _: BChange => ()
        case fac: AChange => {
           propagateCellValue(p, fac.v, fbcs)
           propagateCellValue(p, fac.v, ubcs)
           if(!rest.isEmpty && rest.head.p.x == x && rest.head.p.y == y)
            rest.head.old = fac.v
        }
      }
    }
    rest
  }


  def countAByChange(file: scala.io.BufferedSource, fbcs: List[BChange],
                      ubcs: List[BChange], uacs: List[AChange]) = {
    var rest: List[AChange] = uacs.sortBy(c => (c.p.x, c.p.y))
    file.getLines.zipWithIndex.foreach { case (line, x) =>
      rest = processLine(line, x, fbcs, ubcs, rest)
    }
  }
}
