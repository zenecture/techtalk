package hlist

/**
  * @author bogdanski
  * @since 07.06.16
  */

object Liftable extends Lift[Σ] {
  implicit def r[A]: A => Σ[A] = a => Σ(a)
}

object Transformable extends Trans[Σ, Option] {
  implicit def r[A]: (Σ[A]) => Option[A] = a => a.send
}
