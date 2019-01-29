package change

import utils._

abstract class Change(val p: Position, var v: Int) {
  override def toString: String = if(correct) s"${v}" else "P"

  def depends_on(c: Change): Boolean = false


  var affecteds: List[Change] = List()
  var hasChanged: Boolean = false
  var old = -1
  var correct: Boolean = true

  def propagateChange(viewed: List[Change]) = {
    if(v != old) {
      affecteds.foreach { a =>
        a.follow_propagation(this, viewed)
      }
      old = v
      hasChanged = true
    }
  }

  def follow_propagation(c: Change, viewed: List[Change]) = ()

  def changeValue(newValue: Int) = {
    v = newValue
    propagateChange(List())
  }

  def firstPropagation = propagateChange(List(this))
}


class AChange(pos: Position, value: Int) extends Change(pos, value) {
  def this(x: Int, y: Int, v: Int) = this(new Position(x, y), v)
}


class BChange(pos: Position, val b: Block, value: Int, val counted: Int)
extends Change(pos, value) {

  def this(x:Int, y:Int, r1:Int, c1:Int, r2:Int, c2:Int, v:Int, vc:Int) = {
    this(new Position(x, y), new Block(r1, c1, r2, c2), v, vc)
  }

  override def depends_on(c: Change): Boolean = {
    b.contains(c.p)
  }

  override def follow_propagation(c: Change, viewed: List[Change])= {
    if(viewed.contains(this)) {
      var l = viewed
      while(l.head != this) {
        l.head.correct = false
        l = l.tail
      }
      correct = false
    }
    else {
      if(counted == c.v) v = v + 1
      else if(counted == c.old) v = v - 1
      propagateChange(this::viewed)
    }
  }
}
