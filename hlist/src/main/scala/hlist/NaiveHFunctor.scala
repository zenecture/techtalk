package hlist

import HList._
import Alphabet._

/**
  * @author bogdanski
  * @since 01.06.16
  */
trait NaiveHFunctor[A <: HList] {
  def map[B <: HList](fa: A)(f: A => B): B = f(fa)
}

object NaiveHFunctor extends NaiveHFunctor[A :: B :: HNil]
