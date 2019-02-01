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

  def parse(file: io.BufferedSource): List[Change] = {
    file.getLines.map(parseLine).toList
  }

}
