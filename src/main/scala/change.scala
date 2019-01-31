package change

import utils._

abstract class Change(val p: Position, var v: Int) {

  var affecteds: List[Change] = List()
  var valueWithInitialA: Int = 0

  var hasChanged: Boolean = false
  var oldValue = -1
  var correct: Boolean = true
  var oldCorrect: Boolean = true
  var has_changed: Boolean = false

  override def toString: String = if(correct) s"${v}" else "P"
  def depends_on(c: Change): Boolean = false
  def propagate(c: Change, viewed: List[Change]): Unit = ()

  def evaluate = {
    applyChange(List())
  }

  def applyChange(viewed: List[Change]) = {
    affecteds.foreach(_.propagate(this, viewed))
  }

  def propagateError: Unit = {
    if(!correct) return
    correct = false
    has_changed = true
    affecteds.foreach (_.propagateError)
  }

  def init: Unit = {
    v = valueWithInitialA
    hasChanged = false
    correct = true
  }
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

  override def propagate(c: Change, viewed: List[Change]): Unit = {
    if(viewed.contains(this)) {
      propagateError
    }
    else {
      if(counted == c.v) v = v + 1
      else if(counted == c.oldValue) v = v - 1
      else return
      applyChange(this::viewed)
    }
  }

}
