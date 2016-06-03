package ycombinator.Run

import hlist._

/**
  * @author bogdanski
  * @since 03.06.16
  */

object Assert {

  /**
    * Checks if all values of [[HList]] `l` are equal.
    */
  def apply[L <: HList](l: L): Boolean = l match {
    case hd0 :: (hd1 :: tl) => if (hd0.equals(hd1)) apply(tl) else throw new AssertionError(s"$hd0 != $hd1")
    case HNil => true
  }

}
