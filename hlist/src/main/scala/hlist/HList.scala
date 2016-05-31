package hlist

/**
  * @author bogdanski
  * @since 30.05.16
  */

object HList {
  type HNil = HNil.type
}

trait HList

case class ::[H, +T <: HList](head: H, tail: T) extends HList {
  def ::[I](item: I): I :: this.type = hlist.::(item, this)
}

case object HNil extends HList {
  def ::[I](item: I): I :: this.type = hlist.::(item, HNil)
}

trait Functor1[L <: HList, F[_]] {
  type Res <: HList
}

trait Functor2[L <: HList, F[_], G[_]] {
  type Res <: HList
}

object Functor {
  type Aux1[L <: HList, R <: HList, F[_]] = Functor1[L, F] { type Res = R }
  type Aux2[L <: HList, R <: HList, F[_], G[_]] = Functor2[L, F, G] { type Res = R }
}

trait Lift[F[_]] {
  import HList._
  import Functor._

  implicit def r[A]: A => F[A]

  def lift[L <: HList](l: L)(implicit f: Functor1[L, F]): f.Res = {
    def trav(l: HList): HList = l match {
      case h0 :: tl0 => hlist.::(r(h0), trav(tl0))
      case HNil => HNil
    }
    trav(l).asInstanceOf[f.Res]
  }

  implicit def liftNil: Aux1[HNil, HNil, F] = new Functor1[HNil, F] {
    type Res = HNil
  }

  implicit def liftList[H, L <: HList, R <: HList]
    (implicit f: Aux1[L, R, F], g: H => F[H]): Aux1[H :: L, F[H] :: R, F] = new Functor1[H :: L, F] {
      type Res = F[H] :: R
    }
}

trait Ground[F[_], G[_]] {
  import HList._
  import Functor._

  implicit def r[A]: F[A] => G[A]

  def ground[L <: HList, R <: HList](l: L)(implicit f: Functor2[L, F, G]): f.Res = {
    def trav(l: HList): HList = l match {
      case h0 :: tl0 => hlist.::(r(h0.asInstanceOf[F[Any]]), trav(tl0))
      case HNil => HNil
    }
    trav(l).asInstanceOf[f.Res]
  }

  implicit def groundNil: Aux2[HNil, HNil, F, G] = new Functor2[HNil, F, G] {
    type Res = HNil
  }

  implicit def groundList[H, L <: HList, R <: HList]
    (implicit e: Aux2[L, R, F, G], f: F[H] => G[H]): Aux2[F[H] :: L, G[H] :: R, F, G] = new Functor2[F[H] :: L, F, G] {
      type Res = G[H] :: R
    }
}