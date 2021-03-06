package hlist

import hlist.HList.HNil

/**
  * @author bogdanski
  * @since 30.05.16
  */
object Run extends App {

  LiftTrans.apply
  Mapping.apply
  WithTuple.apply
  LiftAlternative.apply
  TransAlternative.apply

}

object LiftTrans {

  def apply = {
    import Alphabet._
    import Liftable._

    val a = A :: 1 :: "2" :: 3.0 :: HNil
    val b = lift(a)
    val c: Σ[A] :: Σ[Int] :: Σ[String] :: Σ[Double] :: HNil = b
    val d = { import Transformable._ ; trans(b) }
    val e: Option[A] :: Option[Int] :: Option[String] :: Option[Double] :: HNil = d

    val f: Σ[A] :: Σ[B] :: HNil = NaiveHFunctor.map(A :: B :: HNil)(l => Σ(A) :: Σ(B) :: HNil)
    // doesn't compile:
    //val aa = NaiveHFunctor.map(C :: D :: HNil)(l => Σ(C) :: Σ(D) :: HNil)
    val g: Σ[Σ[A]] :: Σ[Σ[B]] :: HNil = lift(f)

    val message = lift(A :: B :: C :: HNil)
    val result: Option[A] :: Option[B] :: Option[C] :: HNil = { import Transformable._; trans(message) }

    println(result.head)
    println(result.tail.head)
    println(result.tail.tail.head)

    // ...
  }

}

object Mapping {

  import Map._
  import Alphabet._
  import ShowableAlphabet._
  import Players._
  import ShowablePlayers._

  def apply = {
    val players = King("Karl") :: Queen("Quintessa") :: Tower("Theodore") :: Pawn("Power Paul")  :: HNil
    val explicit = map(players)
    val prettyPlayers: String :: String :: String :: String :: HNil = explicit
    println(prettyPlayers)

    val helloWorld = H :: E :: L :: L :: O :: __ :: W :: O :: R :: L :: D :: HNil
    val prettyHello = map(helloWorld)
    println(prettyHello)
  }

}

object LiftAlternative {

  import Map._
  import Alphabet._

  def apply = {
    implicit def lift[T]: Func[T, Σ[T]] = new Func[T, Σ[T]] { val f: T => Σ[T] = a => Σ(a) }

    val abc = A :: B :: C :: HNil
    val lifted = map(abc)
    val assert: Σ[A] :: Σ[B] :: Σ[C] :: HNil = lifted
    println(lifted)
  }

}

object TransAlternative {

  import Map._

  def apply = {
    implicit def trans[T]: Func[Σ[T], Option[T]] = new Func[Σ[T], Option[T]] { val f: Σ[T] => Option[T] = a => a.send }

    val nums = Σ(1) :: Σ("2") :: Σ(3.0) :: HNil
    val sent = map(nums)
    val assert: Option[Int] :: Option[String] :: Option[Double] :: HNil = sent
    println(sent)
  }

}

object WithTuple {

  import Alphabet._

  def lift11[_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11]
  (m: (_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11)) = {
    def l[A](a: A) = Σ(a)
    (l(m._1), l(m._2), l(m._3), l(m._4), l(m._5), l(m._6), l(m._7),
      l(m._8), l(m._9), l(m._10), l(m._11))
  }

  def boilerplate1(m: (Σ[H], Σ[E], Σ[L], Σ[L], Σ[O], Σ[__], Σ[W], Σ[O], Σ[R], Σ[L], Σ[D])) =
    (m._1.send, m._2.send, m._3.send, m._4.send, m._5.send, m._6.send, m._7.send,
      m._8.send, m._9.send, m._10.send, m._11.send)

  def boilerplate2[_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11]
  (m: (Σ[_1], Σ[_2], Σ[_3], Σ[_4], Σ[_5],
    Σ[_6], Σ[_7], Σ[_8], Σ[_9], Σ[_10], Σ[_11])) = {
    (m._1.send, m._2.send, m._3.send, m._4.send, m._5.send, m._6.send,
      m._7.send, m._8.send, m._9.send, m._10.send, m._11.send)
  }

  def apply = {
    // doesn't compile:
    // val longText = (T, H, I, S, __, I, S, __, A, __, V, E, R, Y, __, L, O, N, G, __, T, E, X, T)

    val message = (H, E, L, L, O, __, W, O, R, L, D)
    val message2 = (Σ(H), Σ(E), Σ(L), Σ(L), Σ(O), Σ(__), Σ(W), Σ(O), Σ(R), Σ(L), Σ(D))
    val message3 = H :: E :: L :: L :: O :: __ :: L :: I :: S :: T :: Nil // = List[Any]
    val prepared: Iterator[Σ[Any]] = message.productIterator.map(char => Σ(char))
    val extracted: Iterator[Option[Any]] = prepared.map(_.send)


    val boiled2 = boilerplate2(Σ(H), Σ(E), Σ(L), Σ(L), Σ(O), Σ(__), Σ(W), Σ(O), Σ(R), Σ(L), Σ(D))
    val cooled = boiled2.productIterator.map({ case o: Option[_] => o }).flatten

    val lifted = lift11(message)
  }

}
