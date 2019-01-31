package user_file_parser

import collection.breakOut

import change._
import cell_parser._
import utils._

object UserFileParser {

  private def parseLine(str: String): Change = {
    val Array(x, y, cell) = str.split(" ", 3)
    return CellParser.parse(x.toInt, y.toInt, cell)
  }

  def parse(fileName: String): List[Change] = {
    Resource.using(io.Source.fromFile(fileName)) { file =>
      file.getLines.map(parseLine).toList
    }
  }

}
