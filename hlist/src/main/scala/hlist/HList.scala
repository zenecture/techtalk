package hlist

/**
  * @author bogdanski
  * @since 30.05.16
  */

object HList {
  type HNil = HNil.type
}

trait HList {
  def ::[H](head: H): H :: this.type = hlist.::(head, tail = this)
}

case class ::[H, +T <: HList](head: H, tail: T) extends HList

case object HNil extends HList

trait Lift[F[_]] {
  import HList._
  import HFunctor._

  implicit def r[A]: A => F[A]

  def lift[L <: HList](l: L)(implicit f: HFunctor1[L, F]): f.Res = {
    def trav(l: HList): HList = l match {
      case head :: tail => hlist.::(r(head), trav(tail))
      case HNil => HNil
    }
    trav(l).asInstanceOf[f.Res]
  }

  implicit def liftNil: Aux1[HNil, HNil, F] = new HFunctor1[HNil, F] {
    type Res = HNil
  }

  implicit def liftList[H, L <: HList, R <: HList]
    (implicit f: Aux1[L, R, F], g: H => F[H]): Aux1[H :: L, F[H] :: R, F] = new HFunctor1[H :: L, F] {
      type Res = F[H] :: R
    }
}

trait Trans[F[_], G[_]] {
  import HList._
  import HFunctor._

  implicit def r[A]: F[A] => G[A]

  def trans[L <: HList](l: L)(implicit f: HFunctor2[L, F, G]): f.Res = {
    def trav(l: HList): HList = l match {
      case head :: tail => hlist.::(r(head.asInstanceOf[F[Any]]), trav(tail))
      case HNil => HNil
    }
    trav(l).asInstanceOf[f.Res]
  }

  implicit def transHNil: Aux2[HNil, HNil, F, G] = new HFunctor2[HNil, F, G] {
    type Res = HNil
  }

  implicit def transHList[H, L <: HList, R <: HList]
    (implicit f: Aux2[L, R, F, G], g: F[H] => G[H]): Aux2[F[H] :: L, G[H] :: R, F, G] = new HFunctor2[F[H] :: L, F, G] {
      type Res = G[H] :: R
    }
}

object Map {
  import HList._
  import HMapper._

  def map[L <: HList](l: L)(implicit f: HMapper0[L]): f.Res = {
    def bitrav(l: HList, r: HList): HList = l match {
      case hd0 :: tl0 => r match {
        case (hd1: Func[Any, Any]) :: tl1 => ::(hd1.f(hd0), bitrav(tl0, tl1))
      }
      case HNil => HNil
    }
    bitrav(l, f.fs).asInstanceOf[f.Res]
  }

  implicit def mapHNil[A, B]
    (implicit g: Func[A, B]): Aux0[A :: HNil, B :: HNil] = new HMapper0[A :: HNil] {
      type Res = B :: HNil
      def fs = g :: HNil
  }

  implicit def mapHList[A, B, L <: HList, R <: HList]
    (implicit f: Aux0[L, R], g: Func[A, B]): Aux0[A :: L, B :: R] = new HMapper0[A :: L] {
      type Res = B :: R
      def fs = g :: f.fs
  }

}
