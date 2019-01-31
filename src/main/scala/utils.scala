package utils
import scala.math.Ordering.Implicits._
import change._

class Position(val x: Int, val y: Int) {
  override def toString = s"(${x}, ${y})"

  def >(other: Position): Boolean = (x, y) > (other.x, other.y)
}

class Block(val topLeft: Position, val bottomRight: Position) {

  def this(r1:Int, c1:Int, r2:Int, c2:Int) = {
    this(new Position(r1, c1), new Position(r2, c2))
  }
  def contains(position: Position): Boolean = {
    return position.x >= topLeft.x && position.x <= bottomRight.x &&
           position.y >= topLeft.y && position.y <= bottomRight.y
  }

  def isAfter(p: Position): Boolean = {
    topLeft > p
  }
}

object Change {
  def sortByBlockPosition(l: List[BChange]): List[BChange] = {
    l.sortBy { c =>
      (c.b.topLeft.x, c.b.topLeft.y, c.b.bottomRight.x, c.b.bottomRight.y)
    }
  }

  def split(changes: List[Change]): (List[AChange], List[BChange]) = {
    val (la, lb): (List[Change], List[Change]) = changes.partition {
      case c: AChange => true
      case c: BChange => false
    }
    (la.map {case a: AChange => a}, lb.map {case b: BChange => b})
  }
}


object Resource {
  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
  try f(resource)
  finally resource.close()
}
