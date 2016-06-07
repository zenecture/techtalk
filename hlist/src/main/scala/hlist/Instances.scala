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

object Showable {
  import Alphabet._
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
