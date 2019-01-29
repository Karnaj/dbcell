package printer

import change._
import cell_parser._
import java.io._

object ChangePrinter {
  def toFile(filename: String, changes: List[Change]) =  {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    changes.foreach { c =>
      bw.write(c.toString)
      bw.write("\n")
    }
    bw.close
  }

  def printChange(changes: List[Change]) = {
    changes.foreach { c =>
      if(c.hasChanged) println(c)
    }

  }
}

object CSVPrinter {

  def printLine(bw: BufferedWriter, line: String, x: Int,
                l: List[Change]): List[Change] = {
    var rest: List[Change] = l
    line.split(";").zipWithIndex.foreach { case (cell, y) =>
      if(y != 0) bw.write(";")

      while(!rest.isEmpty && rest.head.p.x <= x && rest.head.p.y < y)
        rest = rest.tail

      if(!rest.isEmpty && rest.head.p.x == x && rest.head.p.y == y)
        bw.write(rest.head.toString)
      else
        bw.write(CellParser.parse(x, y, cell).toString)
    }
    bw.write("\n")
    rest
  }

  def printCSVWithChanges(file: scala.io.BufferedSource, outputName: String,
                          cs: List[Change]) = {
     var rest: List[Change] = cs.sortBy(c => (c.p.x, c.p.y))
     val outputFile = new File(outputName)
     val bw = new BufferedWriter(new FileWriter(outputFile))
     file.getLines.zipWithIndex.foreach { case (line, x) =>
       rest = printLine(bw, line, x, rest)
     }
     bw.close()
  }
}
