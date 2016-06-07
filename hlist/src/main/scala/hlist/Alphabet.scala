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

  implicit object showA extends Func[A, String] { val f = (a: A) => "A" }
  implicit object showB extends Func[B, String] { val f = (b: B) => "B" }
  implicit object showC extends Func[C, String] { val f = (c: C) => "C" }
  implicit object showD extends Func[D, String] { val f = (d: D) => "D" }
  implicit object showE extends Func[E, String] { val f = (e: E) => "E" }
  implicit object showF extends Func[F, String] { val f = (f: F) => "F" }
  implicit object showG extends Func[G, String] { val f = (g: G) => "G" }
  implicit object showH extends Func[H, String] { val f = (h: H) => "H" }
  implicit object showI extends Func[I, String] { val f = (i: I) => "I" }
  implicit object showJ extends Func[J, String] { val f = (j: J) => "J" }
  implicit object showK extends Func[K, String] { val f = (k: K) => "K" }
  implicit object showL extends Func[L, String] { val f = (l: L) => "L" }
  implicit object showM extends Func[M, String] { val f = (m: M) => "M" }
  implicit object showN extends Func[N, String] { val f = (n: N) => "N" }
  implicit object showO extends Func[O, String] { val f = (o: O) => "O" }
  implicit object showP extends Func[P, String] { val f = (p: P) => "P" }
  implicit object showQ extends Func[Q, String] { val f = (q: Q) => "Q" }
  implicit object showR extends Func[R, String] { val f = (r: R) => "R" }
  implicit object showS extends Func[S, String] { val f = (s: S) => "S" }
  implicit object showT extends Func[T, String] { val f = (t: T) => "T" }
  implicit object showU extends Func[U, String] { val f = (u: U) => "U" }
  implicit object showV extends Func[V, String] { val f = (v: V) => "V" }
  implicit object showW extends Func[W, String] { val f = (w: W) => "W" }
  implicit object showX extends Func[X, String] { val f = (x: X) => "X" }
  implicit object showY extends Func[Y, String] { val f = (y: Y) => "Y" }
  implicit object showZ extends Func[Z, String] { val f = (z: Z) => "Z" }
  implicit object show__ extends Func[__, String] { val f = (space: __) => " " }

}

case class Σ[T](item: T) {
  def send: Option[T] = Random.nextInt(2) match {
    case x if x == 0 => None
    case _ => Some(item)
  }
}
