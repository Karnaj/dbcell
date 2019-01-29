package bprocess

import change._


object BProcess {
  def compute(l: List[BChange]) = {
    l.foreach(_.firstPropagation)
  }
}
