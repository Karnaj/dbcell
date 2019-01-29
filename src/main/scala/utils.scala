package utils

class Position(val x: Int, val y: Int) {
  override def toString = s"(${x}, ${y})"
}

class Block(val topLeft: Position, val bottomRight: Position) {

  def this(r1:Int, c1:Int, r2:Int, c2:Int) = {
    this(new Position(r1, c1), new Position(r2, c2))
  }
  def contains(position: Position): Boolean = {
    return position.x >= topLeft.x && position.x <= bottomRight.x &&
           position.y >= topLeft.y && position.y <= bottomRight.y
  }
}

object Resource {
  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
  try f(resource)
  finally resource.close()
}
