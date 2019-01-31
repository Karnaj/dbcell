
import user_file_parser._
import change._
import utils._
import csv_preprocessor._
import printer._
import dependencies._
import evaluator._

import java.io._

object Main {

  def split(changes: List[Change]): (List[AChange], List[BChange]) = {
    val (la, lb): (List[Change], List[Change]) = changes.partition {
      case c: AChange => true
      case c: BChange => false
    }
    (la.map {case a: AChange => a}, lb.map {case b: BChange => b})
  }

  def applyUserCommands(applied: List[Change], toApply: List[Change]): Unit = {
    if(toApply == Nil) return
    val (c, rest) = (toApply.head, toApply.tail)
    val newApplied: List[Change] = Modifier.applyNewChange(c, applied)
    println(s"""after "${c.p.x} ${c.p.y} ${c}" """)
    ChangePrinter.printChange(newApplied)
    applyUserCommands(newApplied, rest)
  }

  def main(args: Array[String]): Unit = {
    if(args.size != 4) throw new Exception

    val ucs: List[Change] = Resource.using(io.Source.fromFile(args(1))) {
      UserFileParser.parse(_)
    }


    val fcbs: List[BChange] = Resource.using(io.Source.fromFile(args(0))) {
      CSVPreProcessor.searchFormulae(_)
    }

    val (ucas, ucbs): (List[AChange], List[BChange]) = split(ucs)

    Resource.using(io.Source.fromFile(args(0))) {
      CSVPreProcessor.countA(_, fcbs, ucbs, ucas)
    }

    Dependencies.compute(fcbs)
    Evaluator.evaluateChanges(fcbs)

    Resource.using(io.Source.fromFile(args(0))) {
     CSVPrinter.printCSVWithChanges(_, args(2), fcbs)
    }

    applyUserCommands(fcbs, ucs)
  }
}
