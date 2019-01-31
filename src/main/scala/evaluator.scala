package evaluator

import change._
import dependencies._
import scala.util.{Try}

object Evaluator {
  def evaluateChanges(l: List[BChange]) = {
    l.foreach(_.evaluate)
  }
}

object Modifier {

  private def removeChange(toDel: Change, l: List[Change]): List[Change]= {
    l.foreach { c =>
      c.dependencies = c.dependencies.filter(c1 => c1 != toDel)
      c.affecteds = c.affecteds.filter(c1 => c1 != toDel)
    }
    l.filter(c => c != toDel)
  }

  private def addChange(c: Change, l: List[Change]): List[Change] = {
    l.find { c1 => c1.p.x == c.p.x && c1.p.y == c.p.y } match {
      case None       => c::l
      case Some(oldC) => {
        c.old = oldC.v
        c.oldCorrect = oldC.correct
        removeChange(oldC, c::l)
      }
    }
  }

  def applyNewChange(c: Change, l: List[Change]): List[Change] = {
    l.foreach(_.hasChanged = false)
    val newApplied: List[Change] = addChange(c, l)
    Dependencies.compute(newApplied)
    c match {
      case _: AChange => ()
      case c: BChange => c.dependencies.foreach { d =>
        if(d.v == c.counted) {
          c.v += 1
        }
      }
    }
    c.evaluate
    return newApplied
  }
}
