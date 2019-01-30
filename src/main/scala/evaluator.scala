package evaluator

import change._


object Evaluator {
  def compute(l: List[BChange]) = {
    l.foreach(_.firstPropagation)
  }
}
