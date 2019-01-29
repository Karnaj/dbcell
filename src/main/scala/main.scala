
import user_file_parser._
import change._
import utils._
import csv_preprocessor._
import csv_count_a._
import printer._
import dependencies._
import bprocess._


object Main {

  def split(changes: List[Change]): (List[AChange], List[BChange]) = {
    val (la, lb): (List[Change], List[Change]) = changes.partition {
      case c: AChange => true
      case c: BChange => false
    }
    (la.map {case a: AChange => a}, lb.map {case b: BChange => b})
  }

  def main(args: Array[String]): Unit = {
    if(args.size != 4) throw new Exception

    val ucs: List[Change] = Resource.using(io.Source.fromFile(args(1))) {
      UserFileParser.parse(_).sortBy { c => (c.p.x, c.p.y) }
    }

    val fcbs: List[BChange] = Resource.using(io.Source.fromFile(args(0))) {
      CSVPreProcessor.processInitialChanges(_).sortBy { c => (c.p.x, c.p.y) }
    }

    val (ucas, ucbs): (List[AChange], List[BChange]) = split(ucs)

    Resource.using(io.Source.fromFile(args(0))) {
      CSVCountA.countAByChange(_, fcbs, ucbs, ucas)
    }


    Dependencies.compute(fcbs)
    BProcess.compute(fcbs)
    /*ChangePrinter.printChange(fcbs)*/

    ChangePrinter.toFile(args(3), fcbs)
    
    Resource.using(io.Source.fromFile(args(0))) {
     CSVPrinter.printCSVWithChanges(_, args(2), fcbs)
    }

  }
}
