package hlist

import HList._
import Alphabet._

/**
  * @author bogdanski
  * @since 01.06.16
  */
trait NaiveHFunctor[L <: HList] {
  def map[R <: HList](fa: L)(f: L => R): R = f(fa)
}

object NaiveHFunctor extends NaiveHFunctor[A :: B :: HNil]
