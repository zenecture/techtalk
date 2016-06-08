package ycombinator.Run

import hlist.HNil

import scala.annotation.tailrec

/**
  * @author bogdanski
  * @since 03.06.16
  */

object Run extends App {
  val a = Factorial(5)
  val b = FactorialTailrec(5)
  val c = FactorialIter(5)
  val d = YCombinator[Int, Int](a => b => if (b > 1) b * a(b - 1) else 1)(5)
  val e = FactorialFunc.apply(5)
  val f = NonRecursiveYCombinator[Int, Int](a => b => if (b > 1) b * a(b - 1) else 1)(5)

  Assert(a :: b :: c :: d :: e :: f :: HNil)
}

object Factorial {
  def apply(n: Int): Int = if (n > 1) n * apply(n - 1) else 1
}

object FactorialTailrec {
  def apply(n: Int): Int = {
    @tailrec def fac(i: Int, k: Int): Int = if (i > 1) fac(i - 1, k * (i - 1)) else k
    fac(n, n)
  }
}

object FactorialIter {
  def apply(n: Int): Int = {
    var k = n
    var i = n - 1
    while (i > 1) {
      k = k * i
      i = i - 1
    }
    k
  }
}

object YCombinator {
  def apply[A, B](f: (A => B) => (A => B)): A => B = f(apply(f))(_)
}

object Overflominator {
  def apply[A, B](f: (A => B) => (A => B)): A => B = f(apply(f))
}

object ExplicitYCombinator {
  def apply[A, B](f: (A => B) => (A => B)): A => B = a => f(apply(f))(a)
}

object FactorialFunc {
  val apply: Int => Int = n => if (n > 1) n * apply(n - 1) else 1
}

object NonRecursiveYCombinator {
  case class F[A, B](l: F[A, B] => A => B) { def apply(r: F[A, B]) = l(r) }
  def apply[A, B] = (f: (A => B) => A => B) => F[A, B](x => f(x(x)(_)))(F(x => f(x(x)(_))))
}