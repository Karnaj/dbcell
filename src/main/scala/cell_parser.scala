package cell_parser


import utils._
import change._

final case class InvalidCellContentException(private val message: String = "")
extends Exception(message)

object CellParser {
  val aCell = """(\d+)""".r
  val bCell = """=#\((\d+), (\d+), (\d+), (\d+), (\d+)\)""".r

  def parse(x: Int, y:Int, cell: String): Change = {
    return cell match {
      case aCell(v)                  => new AChange(x, y, v.toInt)
      case bCell(r1, c1, r2, c2, vc) =>
        new BChange(x, y, r1.toInt, c1.toInt, r2.toInt, c2.toInt, 0, vc.toInt)
      case _ => throw new InvalidCellContentException
    }
  }
}
