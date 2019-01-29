package csv_preprocessor

import change._
import cell_parser._

object CSVPreProcessor {

  def processLine(str: String, x: Int): List[BChange] = {
    str.split(";").zipWithIndex.map { case (cell, y) =>
      CellParser.parse(x, y, cell)
    }.collect { case bc: BChange => bc }.toList
  }


  def processInitialChanges(file: scala.io.BufferedSource) : List[BChange] = {
    file.getLines.zipWithIndex.map { case (line, x) =>
      processLine(line, x)
    }.foldLeft(List[BChange]())(_ ::: _)
  }
}
