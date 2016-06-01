package hlist

/**
  * @author bogdanski
  * @since 01.06.16
  */
trait NaiveHFunctor {
  def map[A <: HList, B <: HList](fa: A)(f: A => B): B = f(fa)
}

object NaiveHFunctor extends NaiveHFunctor
