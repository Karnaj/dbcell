package dependencies

import change._
import utils._

/*object CycleFounder {
  def go(m: Change, path: List[Change], viewed: List[Change]) = {
    m.dependencies.foreach { m1 =>
      if(!m1.viewed) {
        var l
      }
    }
  }

  def findCycles(l: List[BChange]) = {
    l.foreach { m =>
      var viewed: List[Change] = List()
      l.foreach { m1 =>
        if m.block.contains(m1.p) {
          m.dependencies = m1 :: m.dependencies
          viewed = go(m1, List(m), viewed)
        }
      }
    }
  }
}*/

object Dependencies {
  def computeAffected(c: Change, l: List[Change]) =  {
    l.foreach { c1 =>
      if(c1.depends_on(c)) {
        c.affecteds = c1 :: c.affecteds
        c1.dependencies = c :: c1.dependencies
      }
    }
  }

  def compute(l: List[Change]) = {
    l.foreach {c =>
      c.affecteds = Nil
      c.dependencies = Nil
    }
    l.foreach(computeAffected(_, l))
  }
}
