package hlist

import scala.util.Random

/**
  * @author bogdanski
  * @since 30.05.16
  */

object Alphabet {

  type A = A.type
  type B = B.type
  type C = C.type
  type D = D.type
  type E = E.type
  type F = F.type
  type G = G.type
  type H = H.type
  type I = I.type
  type J = J.type
  type K = K.type
  type L = L.type
  type M = M.type
  type N = N.type
  type O = O.type
  type P = P.type
  type Q = Q.type
  type R = R.type
  type S = S.type
  type T = T.type
  type U = U.type
  type V = V.type
  type W = W.type
  type X = X.type
  type Y = Y.type
  type Z = Z.type
  type __ = __.type


  case object A
  case object B
  case object C
  case object D
  case object E
  case object F
  case object G
  case object H
  case object I
  case object J
  case object K
  case object L
  case object M
  case object N
  case object O
  case object P
  case object Q
  case object R
  case object S
  case object T
  case object U
  case object V
  case object W
  case object X
  case object Y
  case object Z
  case object __

}

case class Σ[T](item: T) {
  def send: Option[T] = Random.nextInt(2) match {
    case x if x == 0 => None
    case _ => Some(item)
  }
}

object Liftable extends Lift[Σ] {
  implicit def r[A]: A => Σ[A] = a => Σ(a)
}

object Groundable extends Ground[Σ, Option] {
  implicit def r[A]: (Σ[A]) => Option[A] = a => a.send
}