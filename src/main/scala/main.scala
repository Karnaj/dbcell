
import user_file_parser._
import change._
import utils._
import csv_preprocessor._
import printer._
import dependencies._
import evaluator._

import java.io._

object Main {

  def applyUserCommands(applied: List[Change], toApply: List[Change]): Unit = {
    if(toApply == Nil) return
    val (c, rest) = (toApply.head, toApply.tail)
    val newApplied: List[Change] = Modifier.applyNewChange(c, applied)
    print(s"""after "${c.p.x} ${c.p.y} """)
    println(
      c match {
        case c:AChange => s"""${c}""""
        case c:BChange => {
          s"""=#(${c.b.topLeft.x},
              |${c.b.topLeft.y},
              |${c.b.bottomRight.x},
              |${c.b.bottomRight.y},
              |${c.counted})"""".stripMargin.replaceAll("\n", " ")
        }
      }
    )
    ChangePrinter.printChange(newApplied)
    applyUserCommands(newApplied, rest)
  }

  def main(args: Array[String]): Unit = {
    if(args.size != 4) {
      println("Error, usage : ./ws data.csv user.txt view0.csv changes.txt")
      return
    }

    val ucs: List[Change] = Buffer.using(args(1)) { UserFileParser.parse(_) }
    val (uacs, ubcs): (List[AChange], List[BChange]) = Change.split(ucs)
    val fbcs: List[BChange] = Buffer.using(args(0)) { CSVParser.parse(_) }
    Buffer.using(args(0)) {
      CSVPreProcessor.countInitialValues(_, fbcs ::: ubcs, uacs)
    }

    Dependencies.compute(fbcs)
    Evaluator.evaluateChanges(fbcs)

    Resource.using(io.Source.fromFile(args(0))) {
     CSVPrinter.printCSVWithChanges(_, args(2), fbcs)
    }
    applyUserCommands(fbcs, ucs)
  }
}
