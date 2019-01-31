package user_file_parser

import collection.breakOut

import change._
import cell_parser._

object UserFileParser {

  def parseLine(str: String): Change = {
    val Array(x, y, cell) = str.split(" ", 3)
    return CellParser.parse(x.toInt, y.toInt, cell)
  }

  def parse(file: scala.io.BufferedSource) : List[Change] = {
    file.getLines.map(parseLine).toList
  }
}
