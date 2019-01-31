package change

import utils._

abstract class Change(val p: Position, var v: Int) {
  override def toString: String = if(correct) s"${v}" else "P"

  def depends_on(c: Change): Boolean = false


  var affecteds: List[Change] = List()
  var dependencies: List[Change] = List()
  var hasChanged: Boolean = false
  var old = -1
  var correct: Boolean = true

  def applyChange(viewed: List[Change]) = {
    if(v != old) {
      affecteds.foreach { a =>
        a.propagate(this, viewed)
      }
      old = v
      hasChanged = true
    }
  }

  def propagate(c: Change, viewed: List[Change]) = ()

  def changeValue(newValue: Int) = {
    v = newValue
    applyChange(List(this))
  }

  def propagateSuccess: Unit = {
    affecteds.foreach { a =>
      if(!a.correct && a.dependencies.forall(_.correct)) {
        a.correct = true
        a.hasChanged = true
        a.propagateSuccess
      }

    }
  }

  def propagateError: Unit = {
      affecteds.foreach { a =>
        if(a.correct) {
          a.hasChanged = true
          a.correct = false
          affecteds.foreach(_.propagateError)
        }
      }
    }

  def evaluate = {
    val oldC = correct
    correct = true
    propagateSuccess
    affecteds.foreach { a =>
      a.propagate(this, List(this))
    }
    if(v != old) {
      old = v
      hasChanged = true
    }
    if(correct != oldC) hasChanged = true
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

  override def propagate(c: Change, viewed: List[Change]) = {
    if(viewed.contains(this)) {
      if(correct) hasChanged = true
      correct = false
      propagateError
    }
    else {
      if(counted == c.v) v = v + 1
      else if(counted == c.old) v = v - 1
      applyChange(this::viewed)
      if(correct) propagateSuccess
    }
  }
}
